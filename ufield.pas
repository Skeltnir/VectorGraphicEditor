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
end;
var
  Field: TField;
  FShift: TFloatPoint;


implementation


{ TField }

function TField.SceneToField(APoint: TPoint): TFloatPoint;
begin
  SceneToField.X := APoint.x  / FZoom + FShift.X;
  SceneToField.Y := APoint.y / FZoom + FShift.Y;
end;

function TField.FieldToScene(AFloatPoint: TFloatPoint): TPoint;
begin
  FieldToScene.x := round((AFloatPoint.X - FShift.X) * FZoom);
  FieldToScene.y := round((AFloatPoint.Y  - FShift.Y) * FZoom);
end;
initialization
  Field := TField.Create;
  Field.FZoom := 1;
  FShift.X := 0;
  FShift.Y := 0;

end.

