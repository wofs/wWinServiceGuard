unit vFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

function CurrentDateTime:string;
function CurrentTime:string;

implementation

function CurrentDateTime:string;
begin
  Result:= FormatDateTime('dd.mm.yyyy hh:nn:ss',Now);
end;

function CurrentTime:string;
begin
  Result:= FormatDateTime('hh:nn:ss',Now);
end;

end.

