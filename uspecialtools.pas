unit USpecialTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics,UFloatPoint, UField, Spin;
type

  { TSpecialTool }

  TSpecialTool = class
    FIcon: TBitmap;
    procedure Act(AFloatPoint: TFloatPoint); virtual; abstract;
    procedure ContinueAct(AFloatPoint: TFloatPoint); virtual; abstract;
    procedure Act2(); virtual; abstract;
    constructor Create(AName: String);
  end;

  { THandTool }

  THandTool = class(TSpecialTool)
    procedure Act(AFloatPoint: TFloatPoint); override;
    procedure ContinueAct(AFloatPoint: TFloatPoint); override;
    procedure Act2; override;
    public
      FTemp: TFloatPoint;
  end;


  { TMagnifier }

  TMagnifier = class(TSpecialTool)
    procedure Act(AFloatPoint: TFloatPoint); override;
    procedure Act2; override;
    procedure ContinueAct(AFloatPoint: TFloatPoint); override;
  end;

  var
    SpecialTools: array [0..1] of TSpecialTool;

implementation

{ TSpecialTool }

constructor TSpecialTool.Create(AName: String);
begin
  FIcon := TBitmap.Create;
  FIcon.LoadFromFile('Images\' + AName + '.bmp');
end;

{ TMagnifier }

procedure TMagnifier.Act(AFloatPoint: TFloatPoint);
begin
  if (Field.FZoom < 8) and (Field.FZoom >= 1) then
    begin
      Field.FZoom += 1;
    end;
  if Field.FZoom < 1 then
    begin
      Field.FZoom *= 2;
    end;
end;

procedure TMagnifier.Act2;
begin
  if Field.FZoom <= 1 then
    begin
      Field.FZoom /= 2;
    end;
  if Field.FZoom > 1 then
    begin
      Field.FZoom -= 1;
    end;
end;

procedure TMagnifier.ContinueAct(AFloatPoint: TFloatPoint);
begin

end;



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

procedure THandTool.Act2;
begin

end;

initialization
SpecialTools[0] := THandTool.Create('hand');
SpecialTools[1] := TMagnifier.Create('magnifier');

end.

