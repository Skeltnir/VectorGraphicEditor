unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics;

type

  { TTool }

  TTool = class
    FIcon: TBitmap;
    constructor Create(AName: String);
    procedure StartDrawing(APoint: TPoint); virtual; abstract;
    procedure ContinueDrawing(APoint: TPoint); virtual;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure StartDrawing(APoint: TPoint); override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure StartDrawing(APoint: TPoint); override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure StartDrawing(APoint: TPoint); override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure StartDrawing(APoint: TPoint); override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure StartDrawing(APoint: TPoint); override;
  end;

 var
   Tools: array [0..4] of TTool;
implementation

{ TRoundRectangleTool }

procedure TRoundRectangleTool.StartDrawing(APoint: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint);
end;

{ TEllipseTool }

procedure TEllipseTool.StartDrawing(APoint: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint);
end;

{ TRectangleTool }

procedure TRectangleTool.StartDrawing(APoint: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint);
end;

{ TLineTool }

procedure TLineTool.StartDrawing(APoint: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint);
end;

{ TPencilTool }

procedure TPencilTool.StartDrawing(APoint: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint);
end;

constructor TTool.Create(AName: String);
begin
  FIcon := TBitmap.Create;
  FIcon.LoadFromFile('Images\' + AName + '.bmp');
end;

procedure TTool.ContinueDrawing(APoint: TPoint);
begin
   Figures[High(Figures)].AddPoint(APoint);
end;

initialization
 Tools[0] := TPencilTool.Create('pencil');
 Tools[1] := TLineTool.Create('line');
 Tools[2] := TRectangleTool.Create('rectangle');
 Tools[3] := TEllipseTool.Create('ellipse');
 Tools[4] := TRoundRectangleTool.Create('roundrect');
end.

