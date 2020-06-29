unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libcurl, strutils, fpjson, jsonparser, RegExpr, ShellApi,
  windows;

function ReadFile(fnam: string): string;
function Rand(min,max: integer): Integer;
function VeryBadToLower(str: String): String;
function EncodeUrl(url: string): string;
function DoWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;
function WriteFunction(pBuff: Pointer; size: Integer; nmemb: Integer; pUserData: Pointer): Integer;
procedure WriteFile(fnam: string; txt: string);
function URLArgsEncode(args: array of string): string;
procedure logWrite(str: String);
function JSONArrayIndexOfName(Arr: TJSONArray;Value: Integer): Integer; overload;
function JSONArrayIndexOfName(Arr: TJSONArray;Value: String): Integer; overload;
function RunAsAdmin(const Handle: Hwnd; const Path, Params: string): Boolean;

type
  TArrString = array of string;

type
  TRequests = class
  public
    text: string;
    error: string;
    jsondata: TJSONData;
    bs: TBytesStream;
    hCurl: PCURL;
    code: CURLcode;
    function Get(url: string): string;
    procedure Download(url: string; filename: string);
    function Post(url: string; args: array of string; photo: string = ''): string;
    function Json(): TJSONData;
    procedure Free;
    constructor Create;
end;

implementation
uses
  main;
var
  requests: TRequests;

function RunAsAdmin(const Handle: Hwnd; const Path, Params: string): Boolean;
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(Path);
  sei.lpParameters := PAnsiChar(Params);
  sei.nShow := SW_SHOWNORMAL;
  Result := ShellExecuteExA(@sei);
end;

procedure logWrite(str: String);
  var
    logTime: TDateTime;
  begin
    logTime := now();
    MainWin.LoggerMemo.Lines.Add(format('[%s] %s',
                   [formatDateTime('hh:nn:ss"."zzz', logTime),
                    str]));
  end;

function JSONArrayIndexOfName(Arr: TJSONArray;Value: Integer): Integer; overload;
var
  enum: TJSONEnum;
begin
  for enum in Arr do
      if enum.Value.AsInteger = Value then
      begin
        Result := enum.KeyNum;
        Exit;
      end;
  Result := -1;
end;
function JSONArrayIndexOfName(Arr: TJSONArray;Value: String): Integer; overload;
var
  enum: TJSONEnum;
begin
  for enum in Arr do
      if enum.Value.AsString = Value then
      begin
        Result := enum.KeyNum;
        Exit;
      end;
  Result := -1;
end;

function DynamicArray(Arg1: array of string): TArrString;
var
  ArrStr: String;
  ArrResult: array of string;
begin
  for ArrStr in Arg1 do
  begin
      SetLength(ArrResult,Length(ArrResult)+1);
      ArrResult[Length(ArrResult)-1] := ArrStr;
  end;
  Result := ArrResult;
end;

function writeFunction(pBuff: Pointer; size: Integer; nmemb: Integer; pUserData: Pointer): Integer;
begin
  TStream(pUserData).write(pBuff^, size*nmemb);
  writeFunction := size*nmemb;
end;

procedure TRequests.Free;
begin
  bs.Free;
  curl_easy_cleanup(hCurl);
end;

constructor TRequests.Create;
begin
  hCurl := curl_easy_init();
  if assigned(hCurl) then
  begin
    curl_easy_setopt(hCurl, CURLOPT_VERBOSE, [True]);
    curl_easy_setopt(hCurl, CURLOPT_SSL_VERIFYHOST, [False]);
    curl_easy_setopt(hCurl, CURLOPT_SSL_VERIFYPEER, [False]);
    curl_easy_setopt(hCurl, CURLOPT_NOSIGNAL, [True]);
    curl_easy_setopt(hCurl, CURLOPT_VERBOSE, [0]);
    curl_easy_setopt(hCurl, CURLOPT_WRITEFUNCTION, [@writeFunction]);
    curl_easy_setopt(hCurl, CURLOPT_USERAGENT, [PChar('User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:76.0) Gecko/20100101 Firefox/76.0')]);
  end;
end;

function TRequests.Json(): TJSONData;
begin
  Result := GetJSON(text);
end;

function TRequests.Get(url: string): string;
begin
  bs := TBytesStream.Create();
  curl_easy_setopt(hCurl, CURLOPT_WRITEDATA, [Pointer(bs)]);
  curl_easy_setopt(hCurl, CURLOPT_URL, [PChar(url)]);
  //curl_easy_setopt(hCurl, CURLOPT_PROXY, [PChar('socks5://localhost:9050')]);
  code := curl_easy_perform(hCurl);
  error := String(curl_easy_strerror(code));
  text := String(Pchar(String(bs.Bytes)));

  //if error <> 'No error' then
  //   raise Exception.create(error);

  Result := text;
end;

function TRequests.Post(url: string; args: array of string; photo: string = ''): string;
var
  FirstPost, LastPost:pcurl_httppost;
begin
  bs := TBytesStream.Create();
  curl_easy_setopt(hCurl, CURLOPT_WRITEDATA, [Pointer(bs)]);
  curl_easy_setopt(hCurl, CURLOPT_POST, [1]);
  curl_easy_setopt(hCurl, CURLOPT_URL, [PChar(url)]);
  curl_easy_setopt(hCurl, CURLOPT_POSTFIELDS, [PChar( URLArgsEncode(args) )]);

  if photo <> '' then
  begin
    FirstPost:=nil;
    LastPost:=nil;
    curl_formadd(@FirstPost, @LastPost,
      [CURLFORM_COPYNAME,    'photo',
      CURLFORM_FILE,        Pchar(photo),
      CURLFORM_CONTENTTYPE, 'image/jpeg',
      CURLFORM_END]
    );
    curl_easy_setopt(hCurl, CURLOPT_HTTPPOST, [FirstPost]);
  end;

  code := curl_easy_perform(hCurl);
  error := String(curl_easy_strerror(code));
  text := String(Pchar(String(bs.Bytes)));

  curl_easy_setopt(hCurl, CURLOPT_POST, [0]);
  curl_easy_setopt(hCurl, CURLOPT_POSTFIELDS, [PChar('')]);

  if error <> 'No error' then
     raise Exception.create(error);

  Result := text;
end;

procedure TRequests.Download(url: string; filename: string);
Var
  f : TFileStream;
  DhCurl : pCurl;

begin
  F:=TFileStream.Create(filename,fmCreate);
    DhCurl:= curl_easy_init;
      curl_easy_setopt(DhCurl, CURLOPT_USERAGENT, [PChar('User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:76.0) Gecko/20100101 Firefox/76.0')]);
      curl_easy_setopt(DhCurl, CURLOPT_SSL_VERIFYHOST, [False]);
      curl_easy_setopt(DhCurl, CURLOPT_SSL_VERIFYPEER, [False]);
      curl_easy_setopt(DhCurl,CURLOPT_VERBOSE, [True]);
      curl_easy_setopt(DhCurl,CURLOPT_URL,[pchar(url)]);
      curl_easy_setopt(DhCurl,CURLOPT_WRITEFUNCTION,[@DoWrite]);
      curl_easy_setopt(DhCurl,CURLOPT_WRITEDATA,[Pointer(F)]);
      curl_easy_perform(DhCurl);
      curl_easy_cleanup(DhCurl);
  F.Free;
end;

Function DoWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;
begin
  Result:=TStream(Data).Write(Ptr^,Size*nmemb);
end;

function URLArgsEncode(args: array of string): string;
var
  arg1: integer = 0;
  arg2: integer = 1;
  arg: string = '';
begin
  while True do
  begin
    arg += Format('%s=%s',[args[arg1],encodeUrl(args[arg2])]);
    inc(arg1,2);
    inc(arg2,2);
    if arg2 > Length(args) then Break;
    arg += '&';
  end;
  Result := arg;
end;

function readfile(fnam: string): string;
var
  text: TStringList;
begin
  text := TStringList.Create;
  text.LoadFromFile(fnam);
  Result := text.Text;
end;

procedure writefile(fnam: string; txt: string);
var
  strm: TFileStream;
  n: longint;
begin
  strm := TFileStream.Create(fnam, fmCreate);
  n := Length(txt);
  try
    strm.Position := 0;
    strm.Write(txt[1], n);
  finally
    strm.Free;
  end;
end;

function rand(min,max: integer): Integer;
begin
  Result := random(max-min+1)+min;
end;

function veryBadToLower(str: String): String;
const
  convLowers: Array [0..87] of String = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
      'v', 'w', 'x', 'y', 'z', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
      'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'а', 'б', 'в', 'г', 'д', 'е', 'ё', 'ж',
      'з', 'и', 'й', 'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы',
      'ь', 'э', 'ю', 'я');
  convUppers: Array [0..87] of String = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
      'V', 'W', 'X', 'Y', 'Z', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
      'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж',
      'З', 'И', 'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ъ',
      'Ь', 'Э', 'Ю', 'Я');
var
  i: Integer;
begin
  result := str;
  for i := 0 to 87 do
    result := stringReplace(result, convUppers[i], convLowers[i], [rfReplaceAll]);
end;

function encodeUrl(url: string): string;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-'];
begin
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    if url[x] in SafeMask then
    begin
      sBuff := sBuff + url[x];
    end
    else if url[x] = ' ' then
    begin
      sBuff := sBuff + '+';
    end
    else
    begin
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;

  Result := sBuff;
end;

begin
  requests := TRequests.Create;
end.














