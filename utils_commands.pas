unit utils_commands;

{$mode objfpc}

interface

uses
  Classes, SysUtils, RegExpr;

function check_is_real(input: String): Boolean;
function check_is_string(input: String): Boolean;
function check_valid_constant(input: String): Boolean;
implementation

function check_is_real(input: String): Boolean;
var re: TRegExpr;
begin
  Result:= False;
  re := TRegExpr.Create('^[-]?([0-9]+[.]+)?[0-9]+$');
  if re.Exec(input) then
  begin
    Result:= True;
  end;
end;

function check_is_string(input: String): Boolean;
var re: TRegExpr;
begin
  Result:= False;
  re := TRegExpr.Create('^''(.*?)''$');
  if re.Exec(input) then
  begin
    Result:= True;
  end;
end;

function check_valid_constant(input: String): Boolean;
begin
  Result:=False;
  if (check_is_real(input) or check_is_string(input)) then begin
    Result:=True;
  end;
end;

end.

