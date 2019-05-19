unit uOps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, uUtils, uVars;

type
  TGuuToken = (opSub, opSet, opCall, opPrint, opUnknown);

const
  GuuToken: array[opSub..opPrint] of string = (
    'sub', 'set', 'call', 'print'
  );

type
  TGuuOp = class
    public
      OpType         : TGuuToken;
      OpArgs         : TStringList;
      OpLevel        : Word;
      OpLine         : Cardinal;
      OpUnChangedLine: string;
      NextOp         : TGuuOp;
      OpReg          : Pointer;
      function    Step(StepInto: boolean; CallBacks: TList; Trace: TStringList): TGuuOp;
      constructor Create(LineNum: Cardinal; Line:string);
      destructor  Destroy; override;
  end;

implementation

constructor TGuuOp.Create(LineNum: Cardinal; Line:string);
(*
 * That method parse code line.
 *)
var
  s: string;
begin
  inherited Create;
  OpArgs := TStringList.Create;
  OpLine := LineNum;
  OpUnChangedLine := Line;
  OpLevel := CountLevel(Line);

  NextOp    := nil;
  OpReg     := nil;

  s := GetToken(Line, 1);
  OpType := TGuuToken(AnsiIndexStr(s, GuuToken));
  case OpType of
    opSub  : begin // sub <name>
               s := GetToken(Line, 2);
               if Length(s) > 0 then
                OpArgs.Add(s)
               else
                begin
                  writeln('[Syntax error]: Invalid construction "sub" at line ', OpLine, '.');
                  halt;
                end;

               if Length(GetToken(Line, 3)) > 0 then
                begin
                  writeln('[Syntax error]: Invalid construction "', Line, '" at line ', OpLine, '.');
                  halt;
                end;
             end;

    opSet  : begin // set <var> <value>
               OpArgs.Add(GetToken(Line, 2));
               OpArgs.Add(GetToken(Line, 3));
               if (Length(OpArgs[0]) = 0) or (Length(OpArgs[1]) = 0) or
                  (Length(GetToken(Line, 4)) > 0) then
                begin
                  writeln('[Syntax error]: Invalid construction "', Line, '" at line ', OpLine, '.');
                  halt;
                end
             end;

    opCall : begin // call <name>
               s := GetToken(Line, 2);
               if Length(s) > 0 then
                OpArgs.Add(s)
               else
                begin
                  writeln('[Syntax error]: Invalid construction "call" at line ', OpLine, '.');
                  halt;
                end;

               if Length(GetToken(Line, 3)) > 0 then
                begin
                  writeln('[Syntax error]: Invalid construction "', Line, '" at line ', OpLine, '.');
                  halt;
                end;
             end;

    opPrint: begin // print <var>
               s := GetToken(Line, 2);
               if Length(s) > 0 then
                OpArgs.Add(s)
               else
                begin
                  writeln('[Syntax error]: Invalid construction "print" at line ', OpLine, '.');
                  halt;
                end;

               if Length(GetToken(Line, 3)) > 0 then
                begin
                  writeln('[Syntax error]: Invalid construction "', Line, '" at line ', OpLine, '.');
                  halt;
                end;
             end;
    else
      begin
        writeln('[Syntax error]: Invalid token "', s, '" at line ', OpLine, '.');
        halt;
      end;
  end;
end;

destructor  TGuuOp.Destroy;
begin
  FreeAndNil(OpArgs);
  inherited;
end;

function TGuuOp.Step(StepInto: boolean; CallBacks: TList; Trace: TStringList): TGuuOp;
(*
 * That method execute instruction.
 *)
var
  Op: TGuuOp;
  CBSize: Cardinal;
begin
  case OpType of
    opSub: begin
             Trace.Add('-> Sub "' + OpArgs[0] + '"');
             Result := NextOp;
           end;

    opCall: begin
              if StepInto then
               begin
                 if NextOp <> nil then
                  CallBacks.Add(NextOp);
                 Result := TGuuOp(OpReg);
               end
              else
               begin
                 Op := TGuuOp(OpReg);
                 CBSize := CallBacks.Count;

                 while (Op <> nil) or (CallBacks.Count > CBSize) do
                  begin
                    if Op = nil then
                     begin
                       Op := TGuuOp(CallBacks[CallBacks.Count - 1]);
                       CallBacks.Delete(CallBacks.Count - 1);
                       Trace.Delete(Trace.Count - 1);
                     end;

                    Op := Op.Step(StepInto, CallBacks, Trace);
                  end;

                 Result := NextOp;
               end;
            end;

    opPrint: begin
               writeln(TGuuVar(OpReg).gvName, ' = ', TGuuVar(OpReg).gvVal);
               Result := NextOp;
             end;

    opSet: begin
             TGuuVar(OpReg).gvVal := OpArgs[1];
             Result := NextOp;
           end;
  end;
end;

end.

