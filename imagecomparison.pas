unit ImageComparison;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, PairSplitter,
  ExtCtrls, Types;

type
  TForm2 = class(TForm)
    ImageLeft: TImage;
    ImageRight: TImage;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    StatusBar1: TStatusBar;
    procedure ImageLeftMouseDown(Sender: TObject; Button: TMouseButton; X, Y: Integer);
    procedure ImageLeftMouseMove(Sender: TObject; X, Y: Integer);
    procedure ImageLeftMouseWheel(Sender: TObject; WheelDelta: Integer);
    procedure ImageLeftPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ZoomFactorLeft: Double;
    OffsetXLeft: Integer;
    OffsetYLeft: Integer;
    DraggingLeft: Boolean;
    DragStartXLeft: Integer;
    DragStartYLeft: Integer;
    OriginalImageLeft: TBitmap;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  OriginalImageLeft := TBitmap.Create;
  try
    OriginalImageLeft.LoadFromFile('path/to/your/image.jpg'); // **REPLACE WITH YOUR IMAGE PATH**
  except
    ShowMessage('Error loading image!'); // Handle potential errors
  end;
  ZoomFactorLeft := 1.0;
  OffsetXLeft := 0;
  OffsetYLeft := 0;
  DraggingLeft := False;
end;

procedure TForm2.ImageLeftMouseDown(Sender: TObject; Button: TMouseButton; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    DraggingLeft := True;
    DragStartXLeft := X;
    DragStartYLeft := Y;
  end;
end;

procedure TForm2.ImageLeftMouseMove(Sender: TObject; X, Y: Integer);
begin
  if DraggingLeft then
  begin
    OffsetXLeft := OffsetXLeft + (X - DragStartXLeft);
    OffsetYLeft := OffsetYLeft + (Y - DragStartYLeft);
    DragStartXLeft := X;
    DragStartYLeft := Y;
    ImageLeft.Repaint;
  end;
end;

procedure TForm2.ImageLeftMouseWheel(Sender: TObject; WheelDelta: Integer);
const
  ZOOM_STEP = 0.1;
begin
  ZoomFactorLeft := ZoomFactorLeft * (1 + WheelDelta * ZOOM_STEP / 120);
  if ZoomFactorLeft < 0.1 then ZoomFactorLeft := 0.1;
  if ZoomFactorLeft > 10.0 then ZoomFactorLeft := 10.0;
  ImageLeft.Repaint;
end;

procedure TForm2.ImageLeftPaint(Sender: TObject);
var
  SourceRect, DestRect: TRect;
  ScaledWidth, ScaledHeight: Integer;
begin
  if OriginalImageLeft = nil then Exit; // Check *after* image is loaded

  ScaledWidth := Round(OriginalImageLeft.Width * ZoomFactorLeft);
  ScaledHeight := Round(OriginalImageLeft.Height * ZoomFactorLeft);

  // Correct SourceRect calculation:
  SourceRect.Left := Round(-OffsetXLeft / ZoomFactorLeft);
  SourceRect.Top := Round(-OffsetYLeft / ZoomFactorLeft);
  SourceRect.Right := SourceRect.Left + Round(ImageLeft.Width / ZoomFactorLeft);
  SourceRect.Bottom := SourceRect.Top + Round(ImageLeft.Height / ZoomFactorLeft);

  // Keep SourceRect within bounds:
  if SourceRect.Left < 0 then SourceRect.Left := 0;
  if SourceRect.Top < 0 then SourceRect.Top := 0;
  if SourceRect.Right > OriginalImageLeft.Width then SourceRect.Right := OriginalImageLeft.Width;
  if SourceRect.Bottom > OriginalImageLeft.Height then SourceRect.Bottom := OriginalImageLeft.Height;

  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Right := ImageLeft.Width;
  DestRect.Bottom := ImageLeft.Height;

  Form2.ImageLeft.Canvas.StretchDraw(DestRect, OriginalImageLeft);
end;

end.
