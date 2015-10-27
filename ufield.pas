unit UField;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFloatPoint;
type



   { TField }

   TField = class
     function SceneToField(APoint: TPoint): TFloatPoint;
     function FieldToScene(AFloatPoint: TFloatPoint): TPoint;
     public
       FZoom: Double;
       FFloatCenter: TFloatPoint;
       FCenter: TPoint;
end;
var
  Field: TField;


implementation


{ TField }

function TField.SceneToField(APoint: TPoint): TFloatPoint;
begin
  SceneToField.X := APoint.x  / FZoom + FCenter.x;
  SceneToField.Y := APoint.y / FZoom + FCenter.y;
end;

function TField.FieldToScene(AFloatPoint: TFloatPoint): TPoint;
begin
  FieldToScene.x := round((AFloatPoint.X - FFloatCenter.X) * FZoom);
  FieldToScene.y := round((AFloatPoint.Y  - FFloatCenter.Y) * FZoom);
end;
initialization
  Field := TField.Create;
  Field.FZoom := 1;

end.

