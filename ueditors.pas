unit UEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Spin, UFigures, typinfo,
  Controls;

type

  ToInvalidate = procedure of object;
  { TEditor }

  TEditor = class
    constructor Create(AName: String);
    procedure Change(Sender: TObject); virtual; abstract;
    procedure ApplyTo(AFigure: TFigure); virtual;
    procedure Show( APanel: TPanel;
      AWidth, AHeight, AX, AY: Integer);virtual;abstract;
    procedure Hide; virtual;abstract;
    destructor Destroy; override;
    public
      FIsShown: Boolean;
      FName: String;
      FValue: Variant;
  end;



  { TPenWidth }

  { TPenWidthEdt }

  TPenWidthEdt = class(TEditor)
    procedure Change(Sender: TObject); override;
    procedure ApplyTo(AFigure: TFigure); override;
    procedure Show( APanel: TPanel; AWidth, AHeight, AX, AY: Integer);
      override;
    procedure Hide; override;
    destructor Destroy;override;
      private
        FEditor: TSpinEdit;
  end;


  { TPenStyleEdt }

  TPenStyleEdt = class(TEditor)
    procedure Change(Sender: TObject); override;
    procedure ApplyTo(AFigure: TFigure); override;
    procedure Show( APanel: TPanel; AWidth, AHeight, AX, AY: Integer);
      override;
    procedure Hide; override;
    destructor Destroy;override ;
      private
        FEditor: TComboBox;
  end;


  { TBrushStyleEdt }

  TBrushStyleEdt = class(TEditor)
    procedure Change(Sender: TObject); override;
    procedure ApplyTo(AFigure: TFigure); override;
    procedure Show( APanel: TPanel; AWidth, AHeight, AX, AY: Integer);
      override;
    procedure Hide; override;
    destructor Destroy;override;
      private
        FEditor: TComboBox;
  end;


  { TRadiusX }

  TRadiusX = class(TEditor)
    procedure Change(Sender: TObject); override;
    procedure ApplyTo(AFigure: TFigure); override;
    procedure Show( APanel: TPanel; AWidth, AHeight, AX, AY: Integer);
      override;
    procedure Hide; override;
    destructor Destroy;override;
      private
        FEditor: TSpinEdit;
  end;


  { TRadiusY }

  TRadiusY = class(TEditor)
    procedure Change(Sender: TObject); override;
    procedure ApplyTo(AFigure: TFigure); override;
    procedure Show( APanel: TPanel; AWidth, AHeight, AX, AY: Integer);
      override;
    procedure Hide; override;
    destructor Destroy;override;
      private
        FEditor: TSpinEdit;
  end;
   TEditors = class of TEditor;



  procedure RegisterEditor(AEditor: TEditors; AName: String);
  var
    RegisteredEditors: array of TEditor;
    ToRepaint: ToInvalidate;

implementation

procedure RegisterEditor(AEditor: TEditors; AName: String);
begin
  SetLength(RegisteredEditors, Length(RegisteredEditors) + 1);
  RegisteredEditors[High(RegisteredEditors)] := AEditor.Create(AName);
end;

{ TRadiusY }

procedure TRadiusY.Change(Sender: TObject);
var
  i: integer;
begin
  FValue := FEditor.Text;
  for i := 0 to High(Figures) do
    begin
      if Figures[i].isSelected then
        begin
          ApplyTo(Figures[i]);
        end;
    end;
  ToRepaint;
end;

procedure TRadiusY.ApplyTo(AFigure: TFigure);
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].FIsShown and (RegisteredEditors[i].FName = 'PRadiusY') then
        begin
          SetPropValue(AFigure, RegisteredEditors[i].FName, RegisteredEditors[i].FValue);
        end;
    end;
end;

procedure TRadiusY.Show( APanel: TPanel; AWidth, AHeight, AX,
  AY: Integer);
begin
  FEditor := TSpinEdit.Create(APanel);
  with FEditor do
    begin
      Top := AY;
      Left := AX;
      Width := AWidth;
      Height := AHeight;
      MaxValue := 100;
      MinValue := 1;
      Parent := APanel;
      Value := FValue;
      OnChange := @Change;
    end;
  FIsShown := True;
end;

procedure TRadiusY.Hide;
begin
  FEditor.Free;
  FIsShown := False;
end;

destructor TRadiusY.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TRadiusX }
procedure TRadiusX.Change(Sender: TObject);
var
  i: integer;
begin
  FValue := FEditor.Text;
  for i := 0 to High(Figures) do
    begin
      if Figures[i].isSelected then
        begin
          ApplyTo(Figures[i]);
        end;
    end;
  ToRepaint;
end;

procedure TRadiusX.ApplyTo(AFigure: TFigure);
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].FIsShown and (RegisteredEditors[i].FName = 'PRadiusX') then
        begin
          SetPropValue(AFigure, RegisteredEditors[i].FName, RegisteredEditors[i].FValue);
        end;
    end;
end;

procedure TRadiusX.Show( APanel: TPanel; AWidth, AHeight, AX,
  AY: Integer);
begin
  FEditor := TSpinEdit.Create(APanel);
  with FEditor do
    begin
      Top := AY;
      Left := AX;
      Width := AWidth;
      Height := AHeight;
      MaxValue := 100;
      MinValue := 1;
      Parent := APanel;
      Value := FValue;
      OnChange := @Change;
    end;
  FIsShown := True;
end;

procedure TRadiusX.Hide;
begin
  FEditor.Free;
  FIsShown := False;
end;

destructor TRadiusX.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TEditor }

constructor TEditor.Create(AName: String);
begin
  FName := AName;
end;

procedure TEditor.ApplyTo(AFigure: TFigure);
begin

end;

destructor TEditor.Destroy;
begin
  inherited Destroy;
end;

{ TBrushColorStatus }

procedure TBrushStyleEdt.Change(Sender: TObject);
var
  i: integer;
begin
  FValue := FEditor.Text;
  for i := 0 to High(Figures) do
    begin
      if Figures[i].isSelected then
        begin
          ApplyTo(Figures[i]);
        end;
    end;
  ToRepaint;
end;

procedure TBrushStyleEdt.ApplyTo(AFigure: TFigure);
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].FIsShown and (RegisteredEditors[i].FName = 'PBrushStyle') then
        begin
          SetPropValue(AFigure, RegisteredEditors[i].FName, RegisteredEditors[i].FValue);
        end;
    end;
end;

procedure TBrushStyleEdt.Show( APanel: TPanel; AWidth, AHeight,
  AX, AY: Integer);
begin
  FEditor := TComboBox.Create(APanel);
  with FEditor do
    begin
      Top := AY;
      Left := AX;
      Width := AWidth;
      Height := AHeight;
      Parent := APanel;
      AddItem('bsSolid',FEditor);
      AddItem('bsHorizontal',FEditor);
      AddItem('bsVertical',FEditor);
      AddItem('bsFDiagonal',FEditor);
      AddItem('bsBDiagonal',FEditor);
      AddItem('bsClear',FEditor);
      AddItem('bsCross',FEditor);
      AddItem('bsDiagCross',FEditor);
      OnChange := @Change;
    end;
  FIsShown := True;
end;

procedure TBrushStyleEdt.Hide;
begin
  FEditor.Free;
  FIsShown := False;
end;

destructor TBrushStyleEdt.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TPenStyle }

procedure TPenStyleEdt.Change(Sender: TObject);
var
  i: integer;
begin
  FValue := FEditor.Text;
  for i := 0 to High(Figures) do
    begin
      if Figures[i].isSelected then
        begin
          ApplyTo(Figures[i]);
        end;
    end;
  ToRepaint;
end;

procedure TPenStyleEdt.ApplyTo(AFigure: TFigure);
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].FIsShown and (RegisteredEditors[i].FName = 'PPenStyle') then
        begin
          SetPropValue(AFigure, RegisteredEditors[i].FName, RegisteredEditors[i].FValue);
        end;
    end;
end;

procedure TPenStyleEdt.Show( APanel: TPanel; AWidth, AHeight,
  AX, AY: Integer);
begin
  FEditor := TComboBox.Create(APanel);
  with FEditor do
    begin
      Top := AY;
      Left := AX;
      Width := AWidth;
      Height := AHeight;
      Parent := APanel;
      AddItem('psSolid',FEditor);
      AddItem('psDash',FEditor);
      AddItem('psDot',FEditor);
      AddItem('psDashDot',FEditor);
      AddItem('psDashDotDot',FEditor);
      AddItem('psClear',FEditor);
      OnChange := @Change;
    end;
  FIsShown := True;
end;

procedure TPenStyleEdt.Hide;
begin
  FEditor.Free;
  FIsShown := False;
end;

destructor TPenStyleEdt.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TPenWidth }

procedure TPenWidthEdt.Change(Sender: TObject);
var
  i: integer;
begin
  FValue := FEditor.Text;
  for i := 0 to High(Figures) do
    begin
      if Figures[i].isSelected then
        begin
          ApplyTo(Figures[i]);
        end;
    end;
  ToRepaint;
end;

procedure TPenWidthEdt.ApplyTo(AFigure: TFigure);
var
  i: integer;
begin
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].FIsShown and (RegisteredEditors[i].FName = 'PPenWidth') then
        begin
          SetPropValue(AFigure, RegisteredEditors[i].FName, RegisteredEditors[i].FValue);
        end;
    end;
end;

procedure TPenWidthEdt.Show( APanel: TPanel; AWidth, AHeight,
  AX, AY: Integer);
begin
  FEditor := TSpinEdit.Create(APanel);
  with FEditor do
    begin
      Top := AY;
      Left := AX;
      Width := AWidth;
      Height := AHeight;
      MaxValue := 100;
      MinValue := 1;
      Parent := APanel;
      Value := FValue;
      OnChange := @Change;
    end;
  FIsShown := True;
end;

procedure TPenWidthEdt.Hide;
begin
  FEditor.Free;
  FIsShown := False;
end;

destructor TPenWidthEdt.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;
initialization
RegisterEditor(TPenWidthEdt, 'PPenWidth');
RegisterEditor(TPenStyleEdt, 'PPenStyle');
RegisterEditor(TBrushStyleEdt, 'PBrushStyle');
RegisterEditor(TRadiusX, 'PRadiusX');
RegisterEditor(TRadiusY, 'PRadiusY');

end.

