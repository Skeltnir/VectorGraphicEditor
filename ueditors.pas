unit UEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Spin;

type

  TPenWidth = type Integer;
  { TEditor }

  TEditor = class
    constructor Create(AName: String);
    procedure Change(Sender: TObject); virtual; abstract;
    procedure Show( APanel: TPanel;
      AWidth, AHeight, AX, AY: Integer);virtual;abstract;
    procedure Hide; virtual;abstract;
    destructor Destroy; override;
    public
      isShown: Boolean;
      FName: String;
      FValue: Variant;
  end;



  { TPenWidth }

  { TPenWidthEdt }

  TPenWidthEdt = class(TEditor)
    procedure Change(Sender: TObject); override;
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
    procedure Show( APanel: TPanel; AWidth, AHeight, AX, AY: Integer);
      override;
    procedure Hide; override;
    destructor Destroy;override ;
      private
        FEditor: TComboBox;
  end;


  { TBrushColorStatus }

  { TBrushStyleEdt }

  TBrushStyleEdt = class(TEditor)
    procedure Change(Sender: TObject); override;
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

implementation

procedure RegisterEditor(AEditor: TEditors; AName: String);
begin
  SetLength(RegisteredEditors, Length(RegisteredEditors) + 1);
  RegisteredEditors[High(RegisteredEditors)] := AEditor.Create(AName);
end;

{ TRadiusY }

procedure TRadiusY.Change(Sender: TObject);
begin
  FValue := FEditor.Text;
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
  isShown := True;
end;

procedure TRadiusY.Hide;
begin
  FEditor.Free;
  isShown := False;
end;

destructor TRadiusY.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TRadiusX }
procedure TRadiusX.Change(Sender: TObject);
begin
  FValue := FEditor.Text;
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
  isShown := True;
end;

procedure TRadiusX.Hide;
begin
  FEditor.Free;
  isShown := False;
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

destructor TEditor.Destroy;
begin
  inherited Destroy;
end;

{ TBrushColorStatus }

procedure TBrushStyleEdt.Change(Sender: TObject);
begin
  FValue := FEditor.Text;
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
      AddItem('bsHorizonral',FEditor);
      AddItem('bsVertical',FEditor);
      AddItem('bsFDiagonal',FEditor);
      AddItem('bsBDiagonal',FEditor);
      AddItem('bsClear',FEditor);
      AddItem('bsCross',FEditor);
      AddItem('bsDiagCross',FEditor);
      OnChange := @Change;
    end;
  isShown := True;
end;

procedure TBrushStyleEdt.Hide;
begin
  FEditor.Free;
  isShown := False;
end;

destructor TBrushStyleEdt.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TPenStyle }

procedure TPenStyleEdt.Change(Sender: TObject);
begin
  FValue := FEditor.Text;
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
  isShown := True;
end;

procedure TPenStyleEdt.Hide;
begin
  FEditor.Free;
  isShown := False;
end;

destructor TPenStyleEdt.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

{ TPenWidth }

procedure TPenWidthEdt.Change(Sender: TObject);
begin
  FValue := FEditor.Text;
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
  isShown := True;
end;

procedure TPenWidthEdt.Hide;
begin
  FEditor.Free;
  isShown := False;
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

