program guu_dbg;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, StrUtils, uUtils, uOps, uVars, Crt;

var
  LabelNames: TStringList;
  GuuOps, GuuVars: TList;

  SubMain: TGuuOp = nil;
  CurrentOp: TGuuOp;

procedure ParseNext(LineNum: Cardinal; Line: string);
(*
 * Parsing code lines and define variables and labels.
 *)
var
  Op: TGuuOp;
  GV: TGuuVar;
  c: cardinal;
begin
  if Trim(Line) <> '' then
   begin
     Op := TGuuOp.Create(LineNum, Line);
     GuuOps.Add(Op);

     case Op.OpType of
      opSet: begin // define variable and/or optimisation var calling
               GV := nil;
               c := 0;
               while c < GuuVars.Count do
                begin
                  if TGuuVar(GuuVars[c]).gvName = Op.OpArgs[0] then
                   begin
                     GV := TGuuVar(GuuVars[c]);
                     break;
                   end;
                  inc(c);
                end;

               if GV = nil then
                begin
                  GV := TGuuVar.Create(Op.OpArgs[0]);
                  GuuVars.Add(GV);
                end;

               Op.OpReg := GV;
             end;

      opSub: begin // Check for label dublicade declaration
               if Op.OpArgs[0] = 'main' then
                SubMain := Op;

               if LabelNames.IndexOf(Op.OpArgs[0]) <> -1 then
                begin
                  writeln('[Error]: Dublicate sub "', Op.OpArgs[0], '" declaration at line ', Op.OpLine, '.');
                  halt;
                end
               else
                LabelNames.Add(Op.OpArgs[0]);
             end;
     end;
   end;
end;

procedure CheckSemantic;
(*
 * Semantic analyse and calls optimisation.
 *)
var
  c, x: cardinal;
  op: TGuuOp;
begin
  if GuuOps.Count > 0 then
   begin
     if TGuuOp(GuuOps[0]).OpType <> opSub then
      begin
        writeln('[Error]: Operation outside sub at line ', TGuuOp(GuuOps[0]).OpLine, '.');
        halt;
      end;

     c := 0;
     while c < GuuOps.Count do
      begin
        case TGuuOp(GuuOps[c]).OpType of
          opSub:;

          opCall: begin
                    TGuuOp(GuuOps[c - 1]).NextOp := TGuuOp(GuuOps[c]);
                    x := 0;
                    op := nil;
                    while x < GuuOps.Count do
                     begin
                       if TGuuOp(GuuOps[x]).OpType = opSub then
                       if TGuuOp(GuuOps[x]).OpArgs[0] = TGuuOp(GuuOps[c]).OpArgs[0] then
                        begin
                          op := TGuuOp(GuuOps[x]);
                          break;
                        end;
                      inc(x);
                    end;

                   if op <> nil then
                    TGuuOp(GuuOps[c]).OpReg := op
                   else
                    begin
                      writeln('[Error]: Calling to not exist sub "', TGuuOp(GuuOps[c]).OpArgs[0],
                              '" at line ', TGuuOp(GuuOps[c]).OpLine, '.');
                      halt;
                    end;
                 end;

          opPrint: begin
                     TGuuOp(GuuOps[c - 1]).NextOp := TGuuOp(GuuOps[c]);
                     x := 0;
                     while x < GuuVars.Count do
                      begin
                        if TGuuVar(GuuVars[x]).gvName = TGuuOp(GuuOps[c]).OpArgs[0] then
                         begin
                           TGuuOp(GuuOps[c]).OpReg := TGuuVar(GuuVars[x]);
                           break;
                         end;
                        inc(x);
                      end;

                     if TGuuOp(GuuOps[c]).OpReg = nil then
                      begin
                        writeln('[Error]: Variable "', TGuuOp(GuuOps[c]).OpArgs[0],
                                '" for print doesn''t exist at line ', TGuuOp(GuuOps[c]).OpLine, '.');
                      end;
                   end;
          else
            TGuuOp(GuuOps[c - 1]).NextOp := TGuuOp(GuuOps[c]);
        end;
        inc(c);
      end;
   end;
end;

var
  code: TStringList;
  c: Cardinal;
  cmd: string;
  CallBacks: TList;
  Trace: TStringList;
  DebugMode: boolean = true;
begin
  if ParamCount > 0 then
    begin
      // Initialisation

      if not FileExists(ParamStr(1)) then
       begin
         writeln('[Error]: Can''t open file "', ParamStr(1), '".');
         halt;
       end;

      if ParamCount > 1 then
       if LowerCase(ParamStr(2)) = '/run' then
        DebugMode := false;

      code := TStringList.Create;
      code.LoadFromFile(ParamStr(1));

      GuuOps  := TList.Create;
      GuuVars := TList.Create;

      // Parsing and preparing

      LabelNames := TStringList.Create;

      c := 0;
      while c < code.Count do
       begin
         ParseNext(c + 1, Trim(code[c]));
         inc(c);
       end;

      FreeAndNil(LabelNames);

      CheckSemantic;

      if SubMain = nil then
       begin
         writeln('[Error]: Sub "main" doesn''t exist!');
         halt;
       end;


      // Start code execution

      CurrentOp := SubMain;

      CallBacks := TList.Create;
      Trace := TStringList.Create;

      if DebugMode then
       begin
         //Out code and features

         ClrScr;
         writeln('Code for debugging:');
         writeln('.....');
         c := 0;
         while c < code.Count do
          begin
            writeln(FillSpaces(IntToStr(c + 1), 4), '| ', code[c]);
            inc(c);
          end;
         writeln('"""""');

         FreeAndNil(code);

         writeln(sLineBreak,
                 'Features:', sLineBreak,
                 '* i     - step into.', sLineBreak,
                 '* o     - step over.', sLineBreak,
                 '* trace - print stack trace.', sLineBreak,
                 '* var   - print variables list.', sLineBreak,
                 '* x     - exit.', sLineBreak);

         // Execution loop
         while ((CurrentOp <> nil) or (CallBacks.Count > 0)) and (Trace.Count < STACK_SIZE) do
          begin
            write('Line ', CurrentOp.OpLine, ' ~> ');
            readln(cmd);

            // Execute commands
            if cmd = 'i' then
             CurrentOp := CurrentOp.Step(true, CallBacks, Trace)
            else
            if cmd = 'o' then
             CurrentOp := CurrentOp.Step(false, CallBacks, Trace)
            else
            if cmd = 'trace' then
             begin
               writeln('| Trace:');
               c := 0;
               while c < Trace.Count do
                begin
                  writeln('| ', Trace[c]);
                  inc(c);
                end;
               writeln('| -> Line ', CurrentOp.OpLine, ': "', CurrentOp.OpUnChangedLine, '".')
             end
            else
            if cmd = 'var' then
             begin
               writeln('| Variables list:');
               c := 0;
               while c < GuuVars.Count do
                begin
                  writeln('| ', TGuuVar(GuuVars[c]).gvName, ' = ', TGuuVar(GuuVars[c]).gvVal);
                  inc(c);
                end;
             end
            else
            if cmd = 'x' then
             halt;

            // Check for method end & make callback
            if (CurrentOp = nil) and (CallBacks.Count > 0) then
             begin
               CurrentOp := TGuuOp(CallBacks[CallBacks.Count - 1]);
               CallBacks.Delete(CallBacks.Count - 1);
               Trace.Delete(Trace.Count - 1);
             end;
          end;
       end
      else
       begin
         // Only run mode (/run)
         FreeAndNil(code);

         while ((CurrentOp <> nil) or (CallBacks.Count > 0)) and (Trace.Count < STACK_SIZE) do
          begin
            CurrentOp := CurrentOp.Step(false, CallBacks, Trace);
            if (CurrentOp = nil) and (CallBacks.Count > 0) then
             begin
               CurrentOp := TGuuOp(CallBacks[CallBacks.Count - 1]);
               CallBacks.Delete(CallBacks.Count - 1);
               Trace.Delete(Trace.Count - 1);
             end;
          end;
       end;

      if Trace.Count >= STACK_SIZE then
       writeln('[Runtime error]: Stack overflow!');

      FreeAndNil(CallBacks);
      FreeAndNil(Trace);
    end
  else
    writeln(
      'Guu debugger v1.0.', sLineBreak,
      'Author: Pavel Shiryaev (@RoPi0n).', sLineBreak,
      'Run: svmc guu_debugger.vmc <guu source file> [arg]', sLineBreak,
      'Args:', sLineBreak,
      ' /run - Run Guu code.'
    );
end.
