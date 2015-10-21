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
    procedure StartDrawing(APoint: TPoint; A2Width: Integer); virtual; abstract;
    procedure ContinueDrawing(APoint: TPoint); virtual;
  end;

  { TPencilTool }

  TPencilTool = class(TTool)
    procedure StartDrawing(APoint: TPoint; A2Width: Integer); override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
    procedure StartDrawing(APoint: TPoint; A2Width: Integer); override;
  end;


  { TRectangleTool }

  TRectangleTool = class(TLineTool)
    procedure StartDrawing(APoint: TPoint; A2Width: Integer); override;
  end;


  { TEllipseTool }

  TEllipseTool = class(TLineTool)
    procedure StartDrawing(APoint: TPoint; A2Width: Integer); override;
  end;


  { TRoundRectangleTool }

  TRoundRectangleTool = class(TLineTool)
    procedure StartDrawing(APoint: TPoint; A2Width: Integer); override;
  end;

 var
   Tools: array [0..4] of TTool;
implementation

{ TRoundRectangleTool }

procedure TRoundRectangleTool.StartDrawing(APoint: TPoint; A2Width: Integer);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRoundRectangle.Create(APoint, A2Width);
end;

{ TEllipseTool }

procedure TEllipseTool.StartDrawing(APoint: TPoint; A2Width: Integer);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(APoint, A2Width);
end;

{ TRectangleTool }

procedure TRectangleTool.StartDrawing(APoint: TPoint; A2Width: Integer);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(APoint, A2Width);
end;

{ TLineTool }

procedure TLineTool.StartDrawing(APoint: TPoint; A2Width: Integer);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TLine.Create(APoint, A2Width);
end;

{ TPencilTool }

procedure TPencilTool.StartDrawing(APoint: TPoint; A2Width: Integer);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPencil.Create(APoint, A2Width);
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

