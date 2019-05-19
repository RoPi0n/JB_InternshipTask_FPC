unit uVars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGuuVar = class
    public
      gvName: string;
      gvVal: variant;
      constructor Create(VarName: string);
  end;

implementation

constructor TGuuVar.Create(VarName: string);
begin
  inherited Create;
  gvName := VarName;
  gvVal := 0;
end;

end.

