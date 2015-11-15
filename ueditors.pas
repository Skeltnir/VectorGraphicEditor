unit UEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics;

type

  { TEditor }

  TEditor = class
    procedure Change(Sender: TObject); virtual; abstract;
    constructor Create(APanel: TPanel; ALeft, ATop: Integer);virtual;abstract;
    destructor Destroy; override;
  end;



  { TPenWidth }

  TPenWidth = class(TEditor)
    procedure Change(Sender: TObject); override;
    constructor Create(APanel: TPanel; ALeft, ATop: Integer);override ;
    destructor Destroy;override;
      public
        FEditor: TComboBox;
  end;


  { TPenStyleEdt }

  TPenStyleEdt = class(TEditor)
    procedure Change(Sender: TObject); override;
    constructor Create(APanel: TPanel; ALeft, ATop: Integer);override  ;
    destructor Destroy;override ;
      public
        FEditor: TComboBox;
  end;


  { TBrushColorStatus }

  TBrushColorStatus = class(TEditor)
    procedure Change(Sender: TObject); override;
    constructor Create(APanel: TPanel; ALeft, ATop: Integer);override ;
    destructor Destroy;override;
      public
        FEditor: TComboBox;
  end;
  var
    MainScene: TPaintBox;
    BrushStatus: ^Boolean;

implementation

{ TEditor }

destructor TEditor.Destroy;
begin
  inherited Destroy;
end;

{ TBrushColorStatus }

procedure TBrushColorStatus.Change(Sender: TObject);
begin
  Case (Sender as TComboBox).Text of
    '(none)': BrushStatus^ := False;
    'Normal': BrushStatus^ := True;
  end;
end;

constructor TBrushColorStatus.Create(APanel: TPanel; ALeft, ATop: Integer);
begin
  FEditor := TComboBox.Create(APanel);
  With FEditor do
    begin
      Visible := True;
      Width := 100;
      Height := 30;
      Left := ALeft;
      Top := ATop;
      Parent := APanel;
      Enabled := True;
      AddItem('(none)', FEditor);
      AddItem('Normal', FEditor);
      ReadOnly := True;
      OnChange := @Change;
    end;
end;

destructor TBrushColorStatus.Destroy;
begin
  FEditor.Destroy;
  inherited Destroy;
end;

{ TPenStyle }

procedure TPenStyleEdt.Change(Sender: TObject);
begin
  Case (Sender as TComboBox).Text of
    'Solid': MainScene.Canvas.Pen.Style := psSolid;
    'Dash': MainScene.Canvas.Pen.Style := psDash;
    'Dot': MainScene.Canvas.Pen.Style := psDot;
    'DashDot': MainScene.Canvas.Pen.Style := psDashDot;
    'DashDotDot': MainScene.Canvas.Pen.Style := psDashDotDot;
  end;
end;

constructor TPenStyleEdt.Create(APanel: TPanel; ALeft, ATop: Integer);
begin
  FEditor := TComboBox.Create(APanel);
  With FEditor do
    begin
      Visible := True;
      Width := 100;
      Height := 30;
      Left := ALeft;
      Top := ATop;
      Parent := APanel;
      Enabled := True;
      AddItem('Solid', FEditor);
      AddItem('Dash', FEditor);
      AddItem('Dot', FEditor);
      AddItem('DashDot', FEditor);
      AddItem('DashDotDot', FEditor);
      ReadOnly := True;
      OnChange := @Change;
    end;
end;

destructor TPenStyleEdt.Destroy;
begin
  FEditor.Destroy;
  inherited Destroy;
end;

{ TPenWidth }

procedure TPenWidth.Change(Sender: TObject);
begin
  Case (Sender as TComboBox).Text of
    '1': MainScene.Canvas.Pen.Width := 1;
    '3': MainScene.Canvas.Pen.Width := 3;
    '9': MainScene.Canvas.Pen.Width := 9;
    '15': MainScene.Canvas.Pen.Width := 15;
  end;
end;

constructor TPenWidth.Create(APanel: TPanel; ALeft, ATop: Integer);
begin
  FEditor := TComboBox.Create(APanel);
  With FEditor do
  begin
    Visible := True;
    Width := 100;
    Height := 30;
    Left := ALeft;
    Top := ATop;
    Parent := APanel;
    Enabled := True;
    AddItem('1', FEditor);
    AddItem('3', FEditor);
    AddItem('9', FEditor);
    AddItem('15', FEditor);
    ReadOnly := True;
    OnChange := @Change;
  end;
end;

destructor TPenWidth.Destroy;
begin
  FEditor.Destroy;
  inherited Destroy;
end;

end.

