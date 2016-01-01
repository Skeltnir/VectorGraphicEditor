unit USelectTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTools, UFloatPoint, Graphics, LCLIntf, LCLType, UField,
  UFigures, typinfo, UEditors, StdCtrls, ExtCtrls, Dialogs, Controls;
type
  //PVector = ^Vector;
  //Vector = array of string;
  //Names = array of PVector;
  { TSelectTool }

  TSelectTool = class(TTool)
    procedure OnMouseClick_LB(Shift: TShiftState;APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor
      ); override;
    procedure OnMouseClick_RB(Shift: TShiftState;APoint: TFloatPoint; APenColor: TColor; ABrushColor: TColor
      ); override;
    procedure OnMouseMove_LB(Shift: TShiftState;APoint: TFloatPoint); override;
    procedure OnMouseMove_RB(Shift: TShiftState;APoint: TFloatPoint); override;
    function GetFigureProp: integer; override;
    procedure OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint; APanel: TPanel); override;
  public
    FStartPoint: TFloatPoint;
    FSelectionStatus: ( Selected, NotSelected);
  end;


  procedure CrossSets(APanel: TPanel);

implementation

procedure CrossSets(APanel: TPanel);
var
  i, j, y, Count1, Min, MinIndex: integer;
  a: PPropList;
  n: integer;
  names: array of array of string;
  IdenticalNames: array of string;
begin
  y := 0;
  Count1 := 0;
  Min := 100;
  for i := 0 to High(Figures) do
   begin
     if Figures[i].isSelected then
       begin
         n := GetPropList(Figures[i], a);
         if Min > n then
           begin
             Min := n;
             MinIndex := Count1;
           end;
         SetLength(names, Count1 + 1, n);
         for j := 0 to n-1 do
          begin
            names[Count1][j] := a^[j]^.Name;
          end;
         Count1 += 1;
       end;
   end;
  SetLength(IdenticalNames, Min);
      if Length(names) > 0 then
      begin
      For i := 0 to Min - 1 do
      begin
          IdenticalNames[i] := names[MinIndex][i];
        end;
        for i := 0 to Count1 - 1 do;
         for j := 0 to High(IdenticalNames) do
          begin
            if not(IdenticalNames[j] = names[i][j]) then
              begin
                IdenticalNames[j] := '';
                break;
              end;
          end;
        For i := 0 to High(IdenticalNames) do
         For j := 0 to High(RegisteredEditors) do
          begin
            if IdenticalNames[i] = RegisteredEditors[j].FName then
              begin
                RegisteredEditors[j].Show(APanel, 80, 80, 5, y );
                y += 25;
              end;
          end;
      end;
end;

{ TSelectTool }

procedure TSelectTool.OnMouseClick_LB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
var
  i: integer;
begin
  UnSelect;
  FSelectionStatus := NotSelected;
  FStartPoint := APoint;
  if (ssCtrl in Shift) and (Length(Figures) > 0) then
    begin
      for i := 0 to High(Figures) do
        if Figures[i].isSelected = False then
          Figures[i].isPointInside(APoint);
    end
  else
    begin
      if Length(Figures) > 0 then
        for i := 0 to High(Figures) do
          begin
            Figures[i].isPointInside(APoint);
            if Figures[i].isPointInside(APoint) then
              FSelectionStatus := Selected;
          end;
    end;
  if FSelectionStatus = NotSelected then
    begin
      for i := 0 to High(RegisteredEditors) do
       begin
         if RegisteredEditors[i].FIsShown then
           begin
             RegisteredEditors[i].Hide;
           end;
       end;
    end;
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, clBlack, clBlack);
  SetPropValue(Figures[High(Figures)], 'PBrushStyle', bsClear);
  SetPropValue(Figures[High(Figures)], 'PPenStyle', psDash);
end;

procedure TSelectTool.OnMouseClick_RB(Shift: TShiftState; APoint: TFloatPoint;
  APenColor: TColor; ABrushColor: TColor);
begin

end;

procedure TSelectTool.OnMouseMove_LB(Shift: TShiftState; APoint: TFloatPoint);
var
  i: integer;
  a: TRect;
begin
  FSelectionStatus := NotSelected;
  a := Rect(round(FStartPoint.X), round(FStartPoint.Y),
        round(APoint.X), round(APoint.Y));
  Figures[High(Figures)].AddPoint(APoint);
  for i := 0 to High(Figures) - 1 do
   begin
     Figures[i].isIntersected(a);
     if Figures[i].isIntersected(a) then
       FSelectionStatus := Selected;
   end;
end;

procedure TSelectTool.OnMouseMove_RB(Shift: TShiftState; APoint: TFloatPoint);
begin

end;

function TSelectTool.GetFigureProp: integer;
begin
  Result := 0;
end;

procedure TSelectTool.OnMouseUp_LB(Shift: TShiftState; APoint: TFloatPoint;
  APanel: TPanel);
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
       begin
         if RegisteredEditors[i].FIsShown then
           begin
             RegisteredEditors[i].Hide;
           end;
       end;
  Figures[High(Figures)].Free;
  SetLength(Figures, Length(Figures) - 1);
  if FSelectionStatus = Selected then
    CrossSets(APanel);
end;

initialization
RegisterTool(TSelectTool, TFigure);

end.

