unit USpecialTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics,UFloatPoint, UField;
type

  { TSpecialTool }

  TSpecialTool = class
    procedure Act(AFloatPoint: TFloatPoint); virtual; abstract;
    procedure ContinueAct(AFloatPoint: TFloatPoint); virtual; abstract;
  end;

  { THandTool }

  THandTool = class(TSpecialTool)
    procedure Act(AFloatPoint: TFloatPoint); override;
    procedure ContinueAct(AFloatPoint: TFloatPoint); override;
    public
      FTemp: TFloatPoint;
  end;

  var
    SpecialTools: array [-1..-1] of TSpecialTool;

implementation

{ THandTool }

procedure THandTool.Act(AFloatPoint: TFloatPoint);
begin
  FTemp := AFloatPoint;
end;

procedure THandTool.ContinueAct(AFloatPoint: TFloatPoint);
begin
  FShift.X += FTemp.X - AFloatPoint.X;
  FShift.Y += FTemp.Y - AFloatPoint.Y;
end;
initialization
SpecialTools[-1] := THandTool.Create;

end.

