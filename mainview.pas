unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, Spin, UTools, UFigures, UField, UFloatPoint,
  LCLType, Grids, ExtDlgs, UEditors, typinfo, USelectTool, crt;

type
  PHistoryPage = ^THistoryPage;
  THistoryPage = record
    FPage: TStringList;
  end;

  { TMainWindow }

  TMainWindow = class(TForm)
    ColorDialog: TColorDialog;
    FileSaveAs: TMenuItem;
    FileSave: TMenuItem;
    FileOpen: TMenuItem;
    FileNew: TMenuItem;
    FileExport: TMenuItem;
    FileExportToBMP: TMenuItem;
    FileExportPNG: TMenuItem;
    FileExportJPEG: TMenuItem;
    OpenDialog1: TOpenDialog;
    Palette: TDrawGrid;
    InfPanel: TPanel;
    HorizontalScrollBar: TScrollBar;
    AdditionalColor: TPanel;
    MainColor: TPanel;
    MoveBot: TSpeedButton;
    MoveTop: TSpeedButton;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    UnDoBtn: TSpeedButton;
    ReDoBtn: TSpeedButton;
    VerticalScrollBar: TScrollBar;
    ZoomValueLabel: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    FileExit: TMenuItem;
    HelpAbout: TMenuItem;
    ManuDrawing: TMenuItem;
    DrawingClear: TMenuItem;
    ToolsPanel: TPanel;
    Scene: TPaintBox;
    PropertiesPanel: TPanel;
    procedure AdditionalColorClick(Sender: TObject);
    procedure AdditionalColorDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DrawingClearClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure FileExportJPEGClick(Sender: TObject);
    procedure FileExportPNGClick(Sender: TObject);
    procedure FileExportToBMPClick(Sender: TObject);
    procedure FileNewClick(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure FileSaveAsClick(Sender: TObject);
    procedure FileSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure HelpAboutClick(Sender: TObject);
    procedure InfPanelClick(Sender: TObject);
    procedure MainColorClick(Sender: TObject);
    procedure MainColorDblClick(Sender: TObject);
    procedure ManuDrawingClick(Sender: TObject);
    procedure MoveBotClick(Sender: TObject);
    procedure MoveTopClick(Sender: TObject);
    procedure PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure SceneMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScenePaint(Sender: TObject);
    procedure ChangeTool(Sender: TObject);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure UnDoBtnClick(Sender: TObject);
    procedure ReDoBtnClick(Sender: TObject);
    procedure VerticalScrollBarChange(Sender: TObject);
    procedure VerticalScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure OnChange;
    procedure MoveUp;
    procedure MoveDown;
    procedure Delete;
    procedure CheckSaveStatus;
    procedure CreateNewHistory;
    procedure AddToHistory;
    procedure ExtractFromHistory;
    procedure UnDo;
    procedure ReDo;
    function SaveMessageDlg: integer;
    procedure LoadFromStringList(AStringList: TStringList);
    function SaveToStringList(AStringList: TStringList): TStringList;
  private
    { private declarations }
  public
    { public declarations }
  end;
const
  Identificator = 'Keltar_Helviett Image';
var
  MainWindow: TMainWindow;
  CurToolIndex: integer;
  CurColorIndex: integer;
  Borders: TFloatPoints;
  Colors: Array[0..4, 0..17] of TColor;
  CurrentFileName: string;
  WasChanged: Boolean;
  WasSaved: Boolean;
  Star: Boolean;
  History: array of PHistoryPage;
  CurrentHistoryPageIndex: integer;
  CurrentSavedStateIndex: integer;
  IndexOfSavedState: integer;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
var
  newButton: TSpeedButton;
  i, x, y, btn: integer;
begin
  Canva := @Scene.Canvas;
  CurToolIndex := 0;
  x := 5;
  y := 5;
  for i := 0 to High(Tools) do
    begin
      newButton := TSpeedButton.Create(ToolsPanel);
      newButton.Parent := ToolsPanel;
      newButton.Left := x;
      newButton.Top := y;
      newButton.Width := 40;
      newButton.Height := 40;
      newButton.Tag := i;
      Tools[i].FIcon.LoadFromFile('Images\' + IntToStr(i + 1) + '.bmp');
      newButton.Glyph := Tools[i].FIcon;
      newButton.OnClick := @ChangeTool;
      x += 45;
      if i = 0 then
        newButton.Click;
    end;
  VerticalScrollBar.Min := 0;
  VerticalScrollBar.Max := 0;
  VerticalScrollBar.Position := 0;
  HorizontalScrollBar.Min := 0;
  HorizontalScrollBar.Max := 0;
  HorizontalScrollBar.Position := 0;
  MainColor.Tag := 0;
  AdditionalColor.Tag := 1;
  CurColorIndex := 0;
  ToRepaint := @Scene.Invalidate;
  CurrentFileName := 'Nameless.khi';
  Caption := CurrentFileName;
  WasChanged := False;
  WasSaved := False;
  SetLength(History, Length(History) + 1);
  History[High(History)] := new(PHistoryPage);
  History[High(History)]^.FPage := TStringList.Create;
  CurrentHistoryPageIndex := High(History);
  CurrentSavedStateIndex := CurrentHistoryPageIndex;
  AddFigureToHistory := @AddToHistory;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_PRIOR) and (ssShift in Shift) then
    begin
      MoveTop.Click;
      AddToHistory;
      exit;
    end;
  if (Key = VK_NEXT) and (ssShift in Shift) then
    begin
      MoveBot.Click;
      AddToHistory;
      exit;
    end;
  if (Key = VK_PRIOR) and (ssCtrl in Shift) then
    begin
      MoveUp;
      AddToHistory;
      exit;
    end;
  if (Key = VK_NEXT) and (ssCtrl in Shift) then
    begin
      MoveDown;
      AddToHistory;
      exit;
    end;
  if (Key = VK_DELETE) and (Length(Figures) > 0) then
    begin
      Delete;
      AddToHistory;
      exit;
    end;
  if (ssShift in Shift) and (ssCtrl in Shift) and (Key = VK_Z) then
    begin
      ReDo;
      exit;
    end;
  if (Key = VK_Z) and (ssCtrl in Shift) then
    begin
      UnDo;
      exit;
    end;
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
end;

procedure TMainWindow.FileExitClick(Sender: TObject);
begin
  MainWindow.Close;
end;

procedure TMainWindow.FileExportJPEGClick(Sender: TObject);
var
  i: integer;
  JPEG: TJPEGImage;
begin
  if SaveDialog2.Execute then
    begin
      JPEG := TJPEGImage.Create;
      JPEG.SetSize(Scene.Width, Scene.Height);
      JPEG.Canvas.Brush.Color := clWhite;
      JPEG.Canvas.FillRect(0,0,Scene.Width,Scene.Height);
      for i := 0 to High(Figures) do
        Figures[i].Draw(JPEG.Canvas);
      JPEG.SaveToFile(SaveDialog2.FileName);
      JPEG.Free;
    end;
end;

procedure TMainWindow.FileExportPNGClick(Sender: TObject);
var
  PNG: TPortableNetworkGraphic;
  i: integer;
begin
  if SaveDialog2.Execute then
    begin
      PNG := TPortableNetworkGraphic.Create;
      PNG.SetSize(Scene.Width, Scene.Height);
      PNG.Canvas.Brush.Color := clWhite;
      PNG.Canvas.FillRect(0,0,Scene.Width,Scene.Height);
      for i := 0 to High(Figures) do
        Figures[i].Draw(PNG.Canvas);
      PNG.SaveToFile(SaveDialog2.FileName);
      PNG.Free;
    end;
end;

procedure TMainWindow.FileExportToBMPClick(Sender: TObject);
var
  BMP: TBitmap;
  i: integer;
begin
  if SaveDialog2.Execute then
    begin
      BMP := TBitmap.Create;
      BMP.SetSize(Scene.Width, Scene.Height);
      BMP.Canvas.Brush.Color := clWhite;
      BMP.Canvas.FillRect(0,0,Scene.Width,Scene.Height);
      for i := 0 to High(Figures) do
        Figures[i].Draw(BMP.Canvas);
      BMP.SaveToFile(SaveDialog2.FileName);
      BMP.Free;
    end;
end;

procedure TMainWindow.FileNewClick(Sender: TObject);
var
  btn: integer;
begin
  if SaveMessageDlg = mrCancel then exit;
  CurrentFileName := 'Nameless.khi';
  DrawingClearClick(nil);
  WasChanged := False;
  WasSaved := False;
  Star := False;
  CheckSaveStatus;
end;

procedure TMainWindow.FileOpenClick(Sender: TObject);
var
  i, j, index, btn: integer;
  OpenStr, tmp, tmp2, tmp3: TStringList;
  buf, buf2: string;
begin
  if SaveMessageDlg = mrCancel then exit;
  if OpenDialog1.Execute then
    begin
      OpenStr := TStringList.Create;
      OpenStr.LoadFromFile(OpenDialog1.FileName);
      if OpenStr.Strings[0] <> Identificator then exit;
      LoadFromStringList(OpenStr);
      CurrentFileName := OpenDialog1.FileName;
      WasChanged := False;
      WasSaved := True;
      CheckSaveStatus;
      CreateNewHistory;
      Invalidate;
    end;
end;

procedure TMainWindow.FileSaveAsClick(Sender: TObject);
var
  SaveStr: TStringList;
  i: integer;
begin
  if SaveDialog1.Execute then
    begin
      SaveStr := TStringList.Create;
      SaveStr.Add(Identificator);
      SaveStr := SaveToStringList(SaveStr);
      SaveStr.SaveToFile(SaveDialog1.FileName);
      CurrentFileName := SaveDialog1.FileName;
      WasSaved := True;
      WasChanged := False;
      CurrentSavedStateIndex := CurrentHistoryPageIndex;
      CheckSaveStatus;
    end;
end;

procedure TMainWindow.FileSaveClick(Sender: TObject);
var
  SaveStr: TStringList;
  i: integer;
begin
  if (WasSaved = False) then
    begin
      FileSaveAsClick(self);
      Exit;
    end;
  if (WasSaved = True) then
    begin
      SaveStr := TStringList.Create;
      SaveStr.Add(Identificator);
      SaveStr := SaveToStringList(SaveStr);
      SaveStr.SaveToFile(CurrentFileName);
      WasSaved := True;
      WasChanged := False;
      CurrentSavedStateIndex := CurrentHistoryPageIndex;
      CheckSaveStatus;
    end;
end;

procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  btn: integer;
begin
  CanClose:=true;
    if SaveMessageDlg = mrCancel then CanClose := False;
end;

procedure TMainWindow.DrawingClearClick(Sender: TObject);
var
  i: integer;
begin
  if Length(Figures) > 0 then
    begin
      for i := 0 to High(Figures) do
        begin
          Figures[i].Free;
        end;
      SetLength(Figures, 0);
      Invalidate;
    end;
end;

procedure TMainWindow.Button1Click(Sender: TObject);
begin

end;

procedure TMainWindow.AdditionalColorClick(Sender: TObject);
begin
  CurColorIndex := 1;
  AdditionalColor.BorderStyle := bsSingle;
  MainColor.BorderStyle := bsNone;
end;

procedure TMainWindow.AdditionalColorDblClick(Sender: TObject);
begin
  CurColorIndex := 1;
  AdditionalColor.BorderStyle := bsSingle;
  MainColor.BorderStyle := bsNone;
  ColorDialog.Execute;
  AdditionalColor.Color := ColorDialog.Color;
end;

procedure TMainWindow.HelpAboutClick(Sender: TObject);
begin
  ShowMessage('Терехов Дмитрий, Б8103а, 2015 - 2016');
end;

procedure TMainWindow.HorizontalScrollBarChange(Sender: TObject);
begin
  Field.FShift.X := HorizontalScrollBar.Position;
  Invalidate;
end;

procedure TMainWindow.UnDoBtnClick(Sender: TObject);
begin
  UnDo;
end;

procedure TMainWindow.ReDoBtnClick(Sender: TObject);
begin
  ReDo;
end;

procedure TMainWindow.InfPanelClick(Sender: TObject);
begin

end;

procedure TMainWindow.MainColorClick(Sender: TObject);
begin
  CurColorIndex := 0;
  AdditionalColor.BorderStyle := bsNone;
  MainColor.BorderStyle := bsSingle;
end;

procedure TMainWindow.MainColorDblClick(Sender: TObject);
begin
  CurColorIndex := 0;
  AdditionalColor.BorderStyle := bsNone;
  MainColor.BorderStyle := bsSingle;
  ColorDialog.Execute;
  MainColor.Color := ColorDialog.Color;
end;

procedure TMainWindow.ManuDrawingClick(Sender: TObject);
begin

end;

procedure TMainWindow.MoveBotClick(Sender: TObject);
var
  i: integer;
  temp: TFigure;
begin
  for i := High(Figures) downto 0 do
    begin
      if (Figures[i].isSelected = True) and ((i-1) > -1) then
        begin
          temp := Figures[i-1];
          Figures[i-1] := Figures[i];
          Figures[i] := temp;
        end;
    end;

  Invalidate;
end;

procedure TMainWindow.MoveTopClick(Sender: TObject);
var
  i: integer;
  temp: TFigure;
begin
  for i := 0 to High(Figures) do
    begin
      if (Figures[i].isSelected = True) and ((i+1) < Length(Figures)) then
        begin
          temp := Figures[i+1];
          Figures[i+1] := Figures[i];
          Figures[i] := temp;
        end;
    end;
  Invalidate;
end;

procedure TMainWindow.PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  r,g,b,i : integer;
begin
  r := 240; g := 240; b := 240;
  for i := 15 downto 0 do
    begin
      if (aCol = i) and (aRow = 4) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       r -= 16;
       g -= 16;
       b -= 16;
    end;
  r := 0; g := 255; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
   r := 85; g := 255; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 255; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 255; g := 255; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 3) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 0; g := 170; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 85; g := 170; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 170; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
   r := 255; g := 170; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 2) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 0; g := 85; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 85; g := 85; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 85; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 255; g := 85; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 1) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 0; g := 0; b := 0;
  for i := 0 to 3 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 85; g := 0; b := 0;
  for i := 4 to 7 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 170; g := 0; b := 0;
  for i := 8 to 11 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
  r := 255; g := 0; b := 0;
  for i := 12 to 15 do
    begin
      if (aCol = i) and (aRow = 0) then
        begin
          Palette.Canvas.Brush.Color := RGBToColor(r,g,b);
          Colors[aRow,i] := RGBToColor(r,g,b);
          Palette.Canvas.FillRect(aRect);
        end;
       b += 85;
    end;
end;

procedure TMainWindow.PaletteSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  i: integer;
begin
  case CurColorIndex of
    0: MainColor.Color := Colors[aRow,aCol];
    1: AdditionalColor.Color := Colors[aRow,aCol];
  end;
  For i := 0 to High(Figures) do
        begin
          if Figures[i].isSelected then
            begin
              SetPropValue(Figures[i], 'PPenColor', MainColor.Color);
              SetPropValue(Figures[i], 'PBrushColor', AdditionalColor.Color);
            end;
        end;
  AddToHistory;
  Invalidate;
end;

procedure TMainWindow.SceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    begin
      Scene.Canvas.Pen.Color := MainColor.Color;
      Scene.Canvas.Brush.Color := AdditionalColor.Color;
      Tools[CurToolIndex].OnMouseClick_LB(
        Shift,
        Field.SceneToField(Point(X, Y)),
        Scene.Canvas.Pen.Color,
        Scene.Canvas.Brush.Color);
      WasChanged := True;
      CheckSaveStatus;
      OnChange;
      Invalidate;
    end;
  if ssRight in Shift then
    begin
       Scene.Canvas.Pen.Color := AdditionalColor.Color;
       Scene.Canvas.Brush.Color := MainColor.Color;
      Tools[CurToolIndex].OnMouseClick_RB(
        Shift,
        Field.SceneToField(Point(X,Y)),
        Scene.Canvas.Pen.Color,
        Scene.Canvas.Brush.Color);
      WasChanged := True;
      CheckSaveStatus;
      OnChange;
      Invalidate;
    end;
end;

procedure TMainWindow.SceneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

  if (ssLeft in Shift) and (Length(Figures) > 0) then
    begin
      Tools[CurToolIndex].OnMouseMove_LB(Shift,Field.SceneToField(Point(X, Y)));
      OnChange;
      Invalidate;
    end;
  if ssRight in Shift then
    begin
      Tools[CurToolIndex].OnMouseMove_RB(Shift,Field.SceneToField(Point(X, Y)));
      OnChange;
      Invalidate;
    end;
end;

procedure TMainWindow.SceneMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Tools[CurToolIndex].OnMouseUp_LB(Shift, Field.SceneToField(Point(X,Y)), PropertiesPanel);
  Invalidate;
  //AddToHistory;
end;

procedure TMainWindow.ScenePaint(Sender: TObject);
var
  i: TFigure;
  a, temp: TFloatPoints;
begin
  if Length(Figures) <> 0 then
    begin
      a[0].X := Figures[0].FPoints[0].X;
      a[1].X := Figures[0].FPoints[0].X;
      a[0].Y := Figures[0].FPoints[0].X;
      a[1].Y := Figures[0].FPoints[0].X;
      for i in Figures do
        begin
          if i.isSelected then
          i.SelectedDraw(Scene.Canvas)
          else
          i.Draw(Scene.Canvas);
          temp := i.GetBorders(i.FPoints);
          if a[0].X < temp[0].X then a[0].X := temp[0].X;
          if a[0].Y < temp[0].Y then a[0].Y := temp[0].Y;
          if a[1].X > temp[1].X then a[1].X := temp[1].X;
          if a[1].Y > temp[1].Y then a[1].Y := temp[1].Y;
        end;
      if a[0].X > Scene.Width then
        begin
          HorizontalScrollBar.Visible := True;
          HorizontalScrollBar.SetParams(HorizontalScrollBar.Position,
                                        HorizontalScrollBar.Min,
                                        round(a[0].X - Scene.Width))
        end
      else
        begin
         HorizontalScrollBar.Visible := False;
          HorizontalScrollBar.SetParams(HorizontalScrollBar.Position,
                                        HorizontalScrollBar.Min,
                                        0)
        end;
      if a[0].Y > Scene.Height then
        begin
          VerticalScrollBar.Visible := True;
          VerticalScrollBar.SetParams(VerticalScrollBar.Position,
                                      VerticalScrollBar.Min,
                                      round(a[0].Y - Scene.Height));
        end;
      if a[1].X < 0 then
        begin
          HorizontalScrollBar.Visible := True;
          HorizontalScrollBar.SetParams(HorizontalScrollBar.Position,
                                        round(a[1].X),
                                        HorizontalScrollBar.Max);
        end;
      if a[1].Y < 0 then
        begin
          VerticalScrollBar.Visible := True;
          VerticalScrollBar.SetParams(VerticalScrollBar.Position,
                                      round(a[1].Y),
                                      VerticalScrollBar.Max);
        end;
    end;
end;

procedure TMainWindow.ChangeTool(Sender: TObject);
var
  i, n, j, y: integer;
begin
  y := 5 ;
  for i := 0 to High(RegisteredEditors) do
    begin
      if RegisteredEditors[i].FIsShown then
        RegisteredEditors[i].Hide;
    end;
  CurToolIndex := (Sender as TSpeedButton).Tag;
  n := Tools[CurToolIndex].GetFigureProp();
  if n > 0 then
  for  i := 0 to n - 1 do
    for j := 0 to High(RegisteredEditors) do
      begin
        if PropList^[i]^.Name = RegisteredEditors[j].FName then
          begin
            RegisteredEditors[j].Show(PropertiesPanel, 80, 80,5, y );
            y += 25;
          end;
      end;
end;

procedure TMainWindow.VerticalScrollBarChange(Sender: TObject);
begin
  Field.FShift.Y := VerticalScrollBar.Position;
  Invalidate;
end;

procedure TMainWindow.VerticalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TMainWindow.OnChange;
begin
  VerticalScrollBar.Position := round(Field.FShift.Y);
  HorizontalScrollBar.Position := round(Field.FShift.X);
  Invalidate;
end;

procedure TMainWindow.MoveUp;
var
  a: array of integer;
  i: integer;
  temp: TFigure;
begin
  if Length(Figures) <> 0 then
    begin
    for i := 0 to High(Figures) do
      begin
        if Figures[i].isSelected = true then
          begin
            SetLength(a, Length(a) + 1);
            a[High(a)] := i;
          end;
      end;
    if Length(a) > 0 then
    for i := High(a) downto 0 do
      begin
        if a[i] + 1 < Length(Figures)then
          begin
            temp := Figures[a[i] + 1];
            Figures[a[i] + 1] := Figures[a[i]];
            Figures[a[i]] := temp;
          end;
      end;
  end;
  Invalidate;
end;

procedure TMainWindow.MoveDown;
var
  a: array of integer;
  i: integer;
  temp: TFigure;
begin
  if Length(Figures) <> 0 then
    begin
    for i := 0 to High(Figures) do
      begin
        if Figures[i].isSelected = true then
          begin
            SetLength(a, Length(a) + 1);
            a[High(a)] := i;
          end;
      end;
    if Length(a) > 0 then
    for i := High(a) downto 0 do
      begin
        if a[i] - 1 > -1 then
          begin
            temp := Figures[a[i] - 1];
            Figures[a[i] - 1] := Figures[a[i]];
            Figures[a[i]] := temp;
          end;
      end;
  end;
  Invalidate;
end;

procedure TMainWindow.Delete;
var
  i, j: integer;
  a: array of TFigure;
begin

  if Length(Figures) > 0 then
    begin
      j := 0;
      for i := 0 to High(Figures) do
        begin
          if not(Figures[i].isSelected) then
            begin
              Figures[j] := Figures[i];
              inc(j);
            end;
        end;
      SetLength(a, j);
      for i := 0 to High(a) do
        begin
          a[i] := Figures[i];
        end;
      Figures := a;
      Invalidate;
    end;
end;

procedure TMainWindow.CheckSaveStatus;
begin
  Caption := CurrentFileName;
  if WasChanged then
    Caption := Caption+'*';
end;

procedure TMainWindow.CreateNewHistory;
var
  i: integer;
begin
  for i := 0 to High(History) do
    Dispose(History[i]);
  SetLength(History, 1);
  History[High(History)] := new(PHistoryPage);
  History[High(History)]^.FPage := TStringList.Create;
  History[High(History)]^.FPage := SaveToStringList(History[High(History)]^.FPage);
  CurrentHistoryPageIndex := High(History);
  CurrentSavedStateIndex:= CurrentHistoryPageIndex;
end;

procedure TMainWindow.AddToHistory;
var
  i: integer;
begin
  if (Length(Figures) = 0) then exit;
  //if (CurrentHistoryPageIndex <> High(History)) then
    begin
      for i := CurrentHistoryPageIndex + 1 to High(History) do
        dispose(History[i]);
      SetLength(History, CurrentHistoryPageIndex + 2);
      History[High(History)] := new(PHistoryPage);
      History[High(History)]^.FPage := TStringList.Create;
      History[High(History)]^.FPage := SaveToStringList(History[High(History)]^.FPage);
      History[High(History)]^.FPage.SaveToFile('D:\\Check.txt');
      CurrentHistoryPageIndex := High(History);
      Exit;
     end;
    exit;
  SetLength(History, Length(History) + 1);
  History[High(History)] := new(PHistoryPage);
  History[High(History)]^.FPage := TStringList.Create;
  History[High(History)]^.FPage := SaveToStringList(History[High(History)]^.FPage);
  CurrentHistoryPageIndex := High(History);
  History[High(History)]^.FPage.SaveToFile('D:\\Check.txt');
end;

procedure TMainWindow.ExtractFromHistory;
var
  i, j, k: integer;
  tmp: TStringList;
  s: String;
begin
  if   History[CurrentHistoryPageIndex]^.FPage.Count  > 0 then
    LoadFromStringList(History[CurrentHistoryPageIndex]^.FPage)
  else
    DrawingClearClick(nil);
  if (CurrentHistoryPageIndex <> CurrentSavedStateIndex)  then
    WasChanged := True
  else
    WasChanged := False;
    CheckSaveStatus;
end;

procedure TMainWindow.UnDo;
begin
  if (CurrentHistoryPageIndex = 0) then
    Exit;
  Dec(CurrentHistoryPageIndex);
  ExtractFromHistory;
  Invalidate;
end;

procedure TMainWindow.ReDo;
begin
  if (CurrentHistoryPageIndex = High(History)) then
    Exit;
  Inc(CurrentHistoryPageIndex);
  ExtractFromHistory;
  Invalidate;
end;

function TMainWindow.SaveMessageDlg: integer;
var
  btn: integer;
begin
  if (WasChanged) then begin
        btn := MessageDlg('File was changed. Save?',mtCustom,
                                    [mbYes,mbNo,mbCancel], 0);
        if btn = mrYes then FileSaveClick(self);
        Result := btn;
      end;
  Result := mrNone;
end;

procedure TMainWindow.LoadFromStringList(AStringList: TStringList);
var
  tmp: TStringList;
  i,j: integer;
begin
 { for i := 0 to High(Figures) do
    Figures[i].Free;
  SetLength(Figures, 0);}
  DrawingClearClick(nil);
  for i := 1 to AStringList.Count - 1 do
    begin
      tmp := TStringList.Create;
      tmp.Delimiter := '{';
      tmp.DelimitedText := AStringList.Strings[i];
      for j := 0 to High(Tools) do
      if tmp[0] = Tools[j].FFigure.ClassName then
        begin
          SetLength(Figures, Length(Figures) + 1);
          Figures[High(Figures)] := Tools[j].FFigure.Create(Field.SceneToField(Point(0,0)),
                                                            clBlack, clBlack);
          Figures[High(Figures)].Load(tmp);
        end;
    end;
end;

function TMainWindow.SaveToStringList(AStringList: TStringList): TStringList;
var
  i: integer;
begin
  AStringList.Add(Identificator);
  for i := 0 to High(Figures) do
    AStringList.Add(Figures[i].Save);
  Result := AStringList;
end;

end.

