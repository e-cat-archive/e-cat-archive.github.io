program JPEGInfo;

{$APPTYPE CONSOLE}

uses Windows;

type
  WordRec = packed record
    Lo, Hi: Byte;
  end;

  Bytes = array[0..0] of Byte;
  LongWords = array[0..255] of Byte;

function ParseExif(const Data; Count: Integer): Boolean;
var
  BigEndian: Boolean;

  function ConvertWordByteOrder(X: Word): Word;
  asm
    test BigEndian,1
    jz @@1
    xchg al,ah
  @@1:
  end;

  function ConvertLongByteOrder(X: Integer): Integer;
  asm
    test BigEndian,1
    jz @@1
    bswap eax
  @@1:
  end;

type
  THeader = record
    ByteOrder: Word;
    Fixed: Word;
    _IFDOffset: Longint;
  end;
  TFieldHeader = record
    Tag: Word;
    _Type: Word;
    _Count: Longint;
    ValueOffset: Longint;
  end;
  PFieldHeaderArray = ^TFieldHeaderArray;
  TFieldHeaderArray = array[0..0] of TFieldHeader;
  TVersion = array[0..3] of Char;
  TCharEncID = array[0..7] of Char;
const
  Sizes: array[1..10] of Integer = (1, 1, 2, 4, 8, 0, 1, 0, 4, 8);
  ACSII = 2;
  SHORT = 3;
  LONG = 4;
  RATIONAL = 5;
var
  I, Offset, FieldCount, Addr, Size, Len, IFDOffset{, X, X1}: Integer;
  Value: Pointer;
  S, S1, Name: string;
  //WS: WideString;
begin
  Result := False;
  if Count >= SizeOf(THeader) then
    with THeader(Data) do
    begin
      case ByteOrder of
        $4949: BigEndian := False;
        $4D4D: BigEndian := True;
      else
        Exit;
      end;
      if ConvertWordByteOrder(Fixed) <> 42 then
        Exit;
      IFDOffset := _IFDOffset;
      while IFDOffset <> 0 do
      begin
        Offset := ConvertLongByteOrder(IFDOffset);
        if Offset > Count - 2 then
          Exit;
        FieldCount := ConvertWordByteOrder(PWord(Integer(@Data) + Offset)^);
        Inc(Offset, 2);
        if Offset > Count - FieldCount * SizeOf(TFieldHeader) then
          Exit;
        Addr := Integer(@Data) + Offset;
        IFDOffset := 0;
        for I := 0 to FieldCount - 1 do
          with PFieldHeaderArray(Addr)^[I] do
          begin
            Tag := ConvertWordByteOrder(Tag);
            _Type := ConvertWordByteOrder(_Type);
            _Count := ConvertLongByteOrder(_Count);
            if _Type in [Low(Sizes)..High(Sizes)] then
              Size := Sizes[_Type]
            else
              Size := 0;
            Len := _Count * Size;
            if Len <= 4 then
              Value := @ValueOffset
            else
            begin
              Offset := ConvertLongByteOrder(ValueOffset);
              if Offset > Count - Len then
                Exit;
              Value := Pointer(Integer(@Data) + Offset);
            end;
            S := '';
            case _Type of
              ACSII: SetString(S, PChar(Value), _Count);
             { SHORT: X := ConvertWordByteOrder(PWord(Value)^);
              LONG: X := ConvertLongByteOrder(PLongWord(Value)^);
              RATIONAL:
                begin
                  X := ConvertLongByteOrder(LongWords(Value^)[0]);
                  X1 := ConvertLongByteOrder(LongWords(Value^)[1]);
                end; }
            else
              //
            end;
            Name := '';
            case Tag of
              $010E: Name := 'Description';
              $010F: Name := 'Manufacturer';
              $0110: Name := 'Model';
              $0131: Name := 'Software';
              $0132: Name := 'Modified';
              $013B: Name := 'Artist';
              $8298: Name := 'Copyright';
              $9000:
                begin   
                  Name := 'Exif version';
                  S := TVersion(Value^);
                  Insert('.', S, 3);
                end;
              $9003: Name := 'Date taken';
              $9004: Name := 'Date digitalized';
             { $9286:
                if _Count >= 8 then
                begin
                  Name := 'User comment';
                  S := TCharEncID(Value^);
                  Inc(PByte(Value), 8);
                  Dec(_Count, 8);
                  if (S = 'ASCII') or (S = '') then
                    SetString(S, PChar(Value), _Count)
                  else
                  if S = 'UNICODE' then
                  begin
                    SetString(WS, PWideChar(Value), _Count div 2);
                    S := WS;
                  end
                  else
                    S := '<Bad encoding>';
                end; }
              $8769: if _Type = 4 then IFDOffset := PLongint(Value)^;
            else
              //Str(Tag, Name);
              //Name := '#' + Name;
            end;
            if Name <> '' then
            begin
              Len := 16 - Length(Name);
              SetLength(S1, Len);
              FillChar(Pointer(S1)^, Len, ' ');
              WriteLn('    ', Name, ': ', S1, S);
            end;
          end;
        WriteLn;
      end;
    end;
  Result := True; 
end;

var
  Clear: Boolean;

function ParseJPGImage(var Data; Count: Integer): Integer;
const
  RST = $D0;
  SOI = $D8;
  EOI = $D9;
  SOS = $DA;
  APP = $E0;
  APP0 = APP + 0;
  APP1 = APP + 1;
  COM = $FE;
  TEM = $01;
var
  P, Dst, Seg: PByte;
  Tag: Byte;
  Len: Word;
  State: Integer;
  I: Integer;
  P1, Start: PChar;
  ID, Comment: string;
  Remove: Boolean;
begin
  Result := 0;
  State := 0;
  P := @Data;
  Dst := @Data;
  while (Count >= 2) and (P^ = $FF) do
  begin
    Seg := P;
    Inc(P);
    Tag := P^;
    Inc(P);
    Dec(Count, 2);
    case State of
      0: if Tag = SOI then State := 1 else Exit;
      1: if Tag = EOI then State := 2;
      2: Exit;
    end;
    Remove := False;
    if Tag in [RST + 0..RST + 7, SOI, EOI, TEM] then
      Len := 0
    else
    begin
      if Count < 2 then
        Break;
      WordRec(Len).Hi := P^;
      Inc(P);
      WordRec(Len).Lo := P^;
      Inc(P);
      P1 := PChar(P);
      if Count < Len then
        Break;
      Inc(P, Len - 2);
      Dec(Count, Len);
      Remove := Clear;
      case Tag of
        APP + 0..APP + 15:
          begin
            Start := P1;
            I := Len - 2;
            while (I > 0) and (P1^ <> #0) do
            begin
              Inc(P1);
              Dec(I);
            end;
            SetString(ID, Start, P1 - Start);
            WriteLn('  ', {'#', Tag - App : 3, ' ', }ID, ' - ', Len + 2, ' bytes');
            if (Tag = APP0) and (ID = 'JFIF') and (Len >= 16) then
            begin
              Remove := False;
              if Clear then
              begin
                PByte(Integer(Seg) + $10)^ := 0;
                PByte(Integer(Seg) + $11)^ := 0;
                Len := 16;
              end;
            end
            else
            if (Tag = APP1) and (ID = 'Exif') and (I >= 2) then
              ParseExif((P1 + 2)^, I - 2);
          end;
        COM:
          begin
            SetString(Comment, P1, Len - 2);
            WriteLn('Comment: ', Comment);
          end;
      else
        Remove := False;
      end;
    end;
    if not Remove then
    begin
      Move(Seg^, Dst^, Len + 2);
      Inc(Dst, Len + 2);
    end;
    if Tag in [RST + 0..RST + 7, SOS] then
      while Count > 0 do
      begin
        if (P^ = $FF) and (Count > 1) and not (PByte(Integer(P) + 1)^ in [0, $FF]) then
          Break;
        Dst^ := P^;
        Inc(P);
        Inc(Dst);
        Dec(Count);
      end;
  end;
  if State = 2 then
    Result := Integer(Dst) - Integer(@Data);
end;

function LowerCase(const S: string): string;
var
  I: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if S[I] in ['A'..'Z'] then
      Result[I] := Chr(Ord(S[I]) + 32)
    else
      Result[I] := S[I];
end;

function CheckExt(const FileName: string; const Exts: array of string): Boolean;
var
  I: Integer;
  Ext: string;
begin
  Result := False;
  I := Length(FileName);
  while (I > 0) and (FileName[I] <> '.') do
    Dec(I);
  if I <= 1 then
    Exit;
  Ext := LowerCase(Copy(FileName, I + 1, MaxInt));
  Result := True;
  for I := 0 to High(Exts) do
    if Ext = LowerCase(Exts[I]) then
      Exit;
  Result := False;
end;

var
  FileName: string;
  F: file;
  P: Pointer;
  Size, OldSize, Removed: Integer;

procedure WriteLnFileSize;
begin
  WriteLn;
  WriteLn('File size: ', (Size + $3FF) shr 10, ' KB');
  WriteLn;
end;

begin
  FileName := ParamStr(1);
  if not CheckExt(FileName, ['jpg', 'jpeg', 'jpe', 'jif', 'jfif']) then
    Exit;
  WriteLn('File: ', FileName);
  Clear := LowerCase(ParamStr(2)) = '/clear';
  if not Clear then
    FileMode := 0;
  Assign(F, FileName);
  Reset(F, 1);
  Size := FileSize(F);
  WriteLnFileSize;
  GetMem(P, Size);
  try
    BlockRead(F, P^, Size);
    OldSize := Size;
    WriteLn('Information: ');
    Size := ParseJPGImage(P^, Size);
    if Clear then
      if Size <> 0 then
      begin
        WriteLn;
        Removed := OldSize - Size;
        WriteLn(Removed, ' bytes removed');
        if Removed <> 0 then
        begin
          WriteLnFileSize;
          Seek(F, 0);
          BlockWrite(F, P^, Size);
          Truncate(F);
        end;
      end
      else
        WriteLn('Bad format');
  finally
    FreeMem(P);
  end;
  CloseFile(F);

  if LowerCase(ParamStr(3)) <> '/q' then
    ReadLn;
end.
