unit uUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt;

  function FillSpaces(s: string; destLen: byte): string;
  function CountLevel(s: string): word;
  function GetToken(s: string; tokenNum: word): string;

implementation

function FillSpaces(s: string; destLen: byte): string;
begin
  while Length(s) < destLen do
   s := s + ' ';
  Result := s;
end;

function CountLevel(s: string): word;
begin
  Result := 0;
  while copy(s, 1, 1) = ' ' do
   begin
     Delete(s, 1, 1);
     Inc(Result);
   end;
end;

function GetToken(s: string; tokenNum: word): string;
var
  p: word;
begin
  s := Trim(s);
  s := StringReplace(s, '  ', ' ', [rfReplaceAll]);

  while tokenNum > 1 do
   begin
     p := Pos(' ', s);
     if p > 0 then
       Delete(s, 1, p)
     else
       begin
         s := '';
         break;
       end;
     dec(tokenNum);
   end;

  p := Pos(' ', s);
  if p > 0 then
    Delete(s, p, Length(s));

  Result := s;
end;

end.

