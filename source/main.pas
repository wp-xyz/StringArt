unit Main;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, IniFiles, IntegerList,
  LCLIntf, LCLType, Graphics, GraphUtil, Types,
  Forms, Controls,Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, Buttons, ExtDlgs,
  FPImage, IntfGraphics, LazCanvas;

const
  //FILE_NAME = '../../samples/Lenna_(test_image).png';
  //FILE_NAME = '../../samples/Jack_Nicholson.png';
//  FILE_NAME = '../../samples/testimg.png';
//  FILE_NAME = '../../samples/portrait-junge-frau-brille.png';
  FILE_NAME = '../../samples/terry-adj.jpg';
//  FILE_NAME = '../../samples/terry-2.jpg';

type
  TDoublePoint = record
    X, Y: Double;
    function Round: TPoint;
  end;
  TDoublePointArray = array of TDoublePoint;

type
  { TMainForm }

  TMainForm = class(TForm)
    btnReset: TButton;
    btnCalculate: TButton;
    btnSave: TButton;
    btnSaveConnections: TButton;
    cbFileNames: TComboBox;
    cgDisplay: TCheckGroup;
    Label4: TLabel;
    lblLength: TLabel;
    seImgDiameter: TFloatSpinEdit;
    gbImgSelection: TGroupBox;
    gbImgPreparation: TGroupBox;
    gbProcess: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    lbUsedNails: TListBox;
    OpenPictureDialog: TOpenPictureDialog;
    OrigImage: TImage;
    Label1: TLabel;
    lblImgSize: TLabel;
    lblMax: TLabel;
    lblNumNails: TLabel;
    lblTotalLineCount: TLabel;
    PageControl: TPageControl;
    PaintBox: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    rbGrayscale: TRadioButton;
    rbMonochrome: TRadioButton;
    SaveDialog: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox: TScrollBox;
    seNumNails: TSpinEdit;
    seNumLines: TSpinEdit;
    btnBrowse: TSpeedButton;
    btnOpen: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TrackBar: TTrackBar;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveConnectionsClick(Sender: TObject);
    procedure cbFileNamesCloseUp(Sender: TObject);
    procedure cbFileNamesDropDown(Sender: TObject);
    procedure cbShowOrigImgChange(Sender: TObject);
    procedure cgDisplayItemClick(Sender: TObject; {%H-}Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbUsedNailsResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure rbGrayscaleChange(Sender: TObject);
    procedure rbMonochromeChange(Sender: TObject);
    procedure seImgDiameterChange(Sender: TObject);
    procedure seNumNailsChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FOrigImg: TLazIntfImage;
    FWorkImg: TLazIntfImage;
    FWorkCanvas: TLazCanvas;
    FNailPos: array of TDoublePoint;
    FUsedNails: TIntegerList;
    FLastNail: Integer;
    FPaintboxMargin: TSize;

    function GetAverageLineGray(ANail1, ANail2, ALineWidth: Integer): Double;
    procedure GetThickLineGray(x1, y1, x2, y2, ALineWidth: Integer;
      var ASum: Int64; var ACount: Integer);
    procedure GetThinLineGray(x1, y1, x2, y2: Integer;
      var ASum: Int64; var ACount: Integer);

  protected
    procedure AddToFileHistory(const AFileName: String);
    procedure DrawNails(AOffset: TPoint);
    procedure DrawStringImg(ACanvas: TCanvas; const ANailPos: TDoublePointArray; AOffset: TPoint);
    function FindDarkestLine(ANail1: Integer; var ANail2: Integer): Boolean;
    procedure InitNails(ACount: Integer);
    procedure InitNails(ACount: Integer; ADiameter: Double; var ANailPos: TDoublePointArray);
    procedure LoadImage(const AFileName: String);
    procedure MakeGrayScale;
    procedure MakeMonochrome;
    function NextNail(ANail: Integer; ADelta: Integer = 1): Integer;
    function NumNails: Integer;
    procedure Process(ANumLines: Integer);
    procedure Reset;
    procedure SaveConnections(const AFileName: String);
    procedure SaveImage(const AFileName: String);
    procedure UpdateResultsPage;
    procedure UpdateTotalLineCount;
    procedure UpdateWireLength;
  public
    procedure ReadIni;
    procedure WriteIni;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math;

const
  NAIL_RADIUS = 2;
  LINE_WIDTH = 3;
  MAX_HISTORY = 20;

  SHOW_STRING_IMAGE = 0;
  SHOW_NAILS = 1;
  SHOW_ORIG_IMAGE = 2;
  SHOW_WORK_IMAGE = 3;

function TDoublePoint.Round: TPoint;
begin
  Result := Point(System.Round(Self.X), System.Round(Self.Y));
end;

function CreateIni: TCustomIniFile;
begin
  Result := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

function TextSize(AText: String; AFont: TFont): TSize;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(AFont);
    Result := bmp.Canvas.TextExtent(AText);
  finally
    bmp.Free;
  end;
end;


{ TMainForm }

procedure TMainForm.AddToFileHistory(const AFileName: String);
var
  idx: Integer;
begin
  idx := cbFileNames.Items.IndexOf(AFileName);
  if idx = -1 then
    cbFileNames.Items.Insert(0, AFileName)
  else
    cbFileNames.Items.Move(idx, 0);
  cbFileNames.Text := AFilename;
end;

procedure TMainForm.btnResetClick(Sender: TObject);
begin
  Reset;
end;

procedure TMainForm.btnBrowseClick(Sender: TObject);
begin
  with OpenPictureDialog do
  begin
    InitialDir := ExtractFileDir(cbFileNames.Text);
    if Execute then
    begin
      cbFileNames.Text := FileName;
      LoadImage(FileName);
    end;
  end;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  LoadImage(cbFileNames.Text);
end;

procedure TMainForm.btnCalculateClick(Sender: TObject);
begin
  Process(seNumLines.Value);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  with SavePictureDialog do
  begin
    if Execute then
      SaveImage(FileName);
  end;
end;

procedure TMainForm.btnSaveConnectionsClick(Sender: TObject);
begin
  with SaveDialog do
  begin
    if Execute then
      SaveConnections(FileName);
  end;
end;

procedure TMainForm.cbFileNamesCloseUp(Sender: TObject);
begin
  cbFileNames.Text := cbFileNames.Items[cbFileNames.ItemIndex];
  LoadImage(cbFileNames.Text);
end;

procedure TMainForm.cbFileNamesDropDown(Sender: TObject);
var
  i: Integer;
  w: Integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(cbFileNames.Font);
    w:= 0;
    for i := 0 to cbFileNames.Items.Count-1 do
      w := Max(w, bmp.Canvas.TextWidth(cbFilenames.Items[i]));
    cbFileNames.ItemWidth := w + GetSystemMetrics(SM_CXVSCROLL)*2;
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.cbShowOrigImgChange(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TMainForm.cgDisplayItemClick(Sender: TObject; Index: integer);
begin
  if (Index = SHOW_ORIG_IMAGE) and cgDisplay.Checked[SHOW_WORK_IMAGE] then
    cgDisplay.Checked[SHOW_WORK_IMAGE] := false
  else
  if (Index = SHOW_WORK_IMAGE) and cgDisplay.Checked[SHOW_ORIG_IMAGE] then
    cgDisplay.Checked[SHOW_ORIG_IMAGE] := false;
  Paintbox.Invalidate;
end;

procedure TMainForm.DrawStringImg(ACanvas: TCanvas;
  const ANailPos: TDoublePointArray; AOffset: TPoint);
var
  i: Integer;
  P: TPoint;
begin
  if FUsedNails.Count <= 1 then
    exit;

  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1; //LINE_WIDTH;
  P := ANailPos[FUsedNails{%H-}[0]].Round + AOffset;
  ACanvas.MoveTo(P);
  for i := 1 to FUsedNails.Count-1 do
  begin
    P := ANailPos[FUsedNails{%H-}[i]].Round + AOffset;
    ACanvas.LineTo(P);
  end;
end;

procedure TMainForm.DrawNails(AOffset: TPoint);
const
  EPS = 1E-6;
  TWO_PI = 2.0 * pi;
  PI_1_2 = pi * 0.5;
  PI_3_2 = pi * 1.5;
  PI_1_4 = pi * 0.25;
  PI_3_4 = pi * 0.75;
  PI_5_4 = pi * 1.25;
  PI_7_4 = pi * 1.75;
var
  i: Integer;
  R: TRect;
  phi: Double;
  x, y: Integer;
  LabelDist: Integer;
  s: String;
  textExt: TSize;
begin
  Paintbox.Canvas.Brush.Color := clBlack;
  if NumNails > 180 then
    LabelDist := 10
  else if NumNails > 90 then
    LabelDist := 5
  else
    LabelDist := 1;
  SetBkMode(Paintbox.Canvas.Handle, TRANSPARENT);
  for i := 0 to High(FNailPos) do
  begin
    R := Rect(-NAIL_RADIUS, -NAIL_RADIUS, NAIL_RADIUS, NAIL_RADIUS);
    OffsetRect(R, round(FNailPos[i].X) + AOffset.X, round(FNailPos[i].Y) + AOffset.Y);
    Paintbox.Canvas.Ellipse(R);
    if i mod LabelDist = 0 then
    begin
      s := IntToStr(i);
      textExt := Paintbox.Canvas.TextExtent(s);
      phi := i / NumNails * TWO_PI;
      // x
      if SameValue(phi, PI_1_2, EPS) or SameValue(phi, PI_3_2, EPS) then
        x := (R.Left + R.Right - textExt.CX) div 2
      else
      if ((phi >= 0) and (phi < PI_1_2)) or ((phi >= PI_3_2) and (phi < TWO_PI)) then
        x := R.Right + NAIL_RADIUS
      else
        x := R.Left - textExt.CX - NAIL_RADIUS;
      // y
      if ((phi >= 0) and (phi < PI_1_4))  or
         ((phi >= PI_7_4) and (phi < TWO_PI)) or
         ((phi >= PI_3_4) and (phi < PI_5_4))
      then
        y := (R.Top + R.Bottom - textExt.CY) div 2
      else
      if (phi < PI) then
        y := R.Top - textExt.CY - NAIL_RADIUS
      else
        y := R.Bottom + NAIL_RADIUS;
      {
      if SameValue(phi, PI_1_2, EPS) then
      begin
        x := (R.Left + R.Right) div 2;
        y := R.Top - textExt.CY - NAIL_RADIUS;
      end else
      if SameValue(phi, PI_3_2, EPS) then
      begin
        x := (R.Left + R.Right) div 2;
        y := R.Bottom + textExt.CY + NAIL_RADIUS;
      end else
      begin
        if (phi > PI_3_2) or (phi < PI_1_2) then
          x := R.Right + NAIL_RADIUS
        else
          x := R.Left - textExt.CX - NAIL_RADIUS;
        if (phi < PI_1_4) or (phi > PI_7_4) or ((phi > PI_3_4) and (phi < PI_5_4)) then
          y := (R.Top + R.Bottom - textExt.CY) div 2
        else
        if (pi <= PI_3_4) then
          y := R.Top - textExt.CY - NAIL_RADIUS
        else
          y := R.Bottom + textExt.CY + NAIL_RADIUS;
      end;
      }
      Paintbox.Canvas.TextOut(x, y, s);
    end;
  end;
end;

function TMainForm.FindDarkestLine(ANail1: Integer; var ANail2: Integer): Boolean;
var
  nail2, nailForDarkestLine: Integer;
  gray, darkestGray: Double;
  i: Integer;
begin
  nailForDarkestLine := -1;
  darkestGray := MaxDouble;
  for i := 0 to High(FNailPos) do
  begin
    nail2 := NextNail(ANail2, i);
    if nail2 = ANail1 then
      continue;
    gray := GetAverageLineGray(ANail1, nail2, 1); //LINE_WIDTH);
    if gray < darkestGray then
    begin
      darkestGray := gray;
      nailForDarkestLine := nail2;
    end;
  end;
  ANail2 := nailForDarkestLine;
  Result := nailForDarkestLine > -1;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FUsedNails := TIntegerList.Create;

  PageControl.TabIndex := 0;
  cgDisplay.Checked[SHOW_STRING_IMAGE] := true;

  ReadIni;
  if ParamCount > 0 then
    cbFileNames.Text := ParamStr(1);

  LoadImage(cbFileNames.Text);
  Reset;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  try
    WriteIni;
  except
  end;
  FWorkCanvas.Free;
  FWorkImg.Free;
  FOrigImg.Free;
  FUsedNails.Free;
  FNailPos := nil;
end;

procedure TMainForm.lbUsedNailsResize(Sender: TObject);
var
  wCol: Integer;
begin
  wCol := TextSize('Index 99999: Nail #999999', lbUsedNails.Font).CX + 20;
  lbUsedNails.Columns := lbUsedNails.ClientWidth div wCol;
end;

function TMainForm.GetAverageLineGray(ANail1, ANail2, ALineWidth: Integer): Double;
var
  P1, P2: TPoint;
  sum: Int64;
  count: Integer;
begin
  if (ANail1 < 0) or (ANail1 > High(FNailPos)) or
     (ANail2 < 0) or (ANail2 > High(FNailPos)) then
  begin
    Result := MaxInt;
    exit;
  end;

  P1 := FNailPos[ANail1].Round;
  P2 := FNailPos[ANail2].Round;

  sum := 0;
  count := 0;
  if ALineWidth = 1 then
    GetThinLineGray(P1.X, P1.Y, P2.X, P2.Y, sum, count)
  else
    GetThickLineGray(P1.X, P1.Y, P2.X, P2.Y, ALineWidth, sum, count);

  Result := sum/count;
end;


procedure TMainForm.GetThickLineGray(x1, y1, x2, y2, ALineWidth: Integer;
  var ASum: Int64; var ACount: Integer);
var
  w1, w2, r: integer;
  MoreHor: boolean;
begin
  // Determine lines above and under
  w1 := ALineWidth div 2;
  w2 := w1;
  if w1 + w2 = ALineWidth then
    dec(w1);

  // Determine slanting
  MoreHor := (abs(x2-x1) < abs(y2-y1));
  if MoreHor then
  begin  // add lines left/right
    for r := 1 to w1 do
      GetThinLineGray(x1-r, y1, x2-r, y2, ASum, ACount);
    for r := 1 to w2 do
      GetThinLineGray(x1+r, y1, x2+r, y2, ASum, ACount);
  end else
  begin  // add lines above/under
    for r := 1 to w1 do
      GetThinLineGray(x1, y1-r, x2, y2-r, ASum, ACount);
    for r := 1 to w2 do
      GetThinLineGray(x1, y1+r, x2, y2+r, ASum, ACount);
  end;
end;

procedure TMainForm.GetThinLineGray(x1, y1, x2, y2: Integer;
  var ASum: Int64; var ACount: Integer);

  function GetPixelGray(x, y: Integer): Integer;
  var
    c: TColor;
  begin
    x := EnsureRange(x, 0, FWorkImg.Width-1);
    y := EnsureRange(y, 0, FWorkImg.Height-1);
    c := FPColorToTColor(FWorkImg.Colors[x, y]);
    Result := ColorToGray(c);
  end;

var
  npixels, xinc1, yinc1, xinc2, yinc2, dx, dy, d, dinc1, dinc2: integer;

  procedure InitLine(x1, y1, x2, y2: Integer);
  begin // precalculations
    dx := abs(x2-x1);
    dy := abs(y2-y1);
    if dx > dy then  // determining independent variable
    begin  // x is independent
      npixels := dx + 1;
      d := (2 * dy) - dx;
      dinc1 := dy * 2;
      dinc2 := (dy - dx) * 2;
      xinc1 := 1;
      xinc2 := 1;
      yinc1 := 0;
      yinc2 := 1;
    end else
    begin  // y is independent
      npixels := dy + 1;
      d := (2 * dx) - dy;
      dinc1 := dx * 2;
      dinc2 := (dx - dy) * 2;
      xinc1 := 0;
      xinc2 := 1;
      yinc1 := 1;
      yinc2 := 1;
    end;
    // going into the correct direction
    if x1 > x2 then
    begin
      xinc1 := - xinc1;
      xinc2 := - xinc2;
    end;
    if y1 > y2 then
    begin
      yinc1 := - yinc1;
      yinc2 := - yinc2;
    end;
  end;

var
  x, y, i: Integer;
begin
  InitLine(x1, y1, x2, y2);
  x := x1;
  y := y1;
  for i := 0 to nPixels-1 do
  begin
    ASum := ASum + GetPixelGray(x, y);
    if d < 0 then
    begin
      d := d + dinc1;
      x := x + xinc1;
      y := y + yinc1;
    end else
    begin
      d := d + dinc2;
      x := x + xinc2;
      y := y + yinc2;
    end;
  end;
  ACount := ACount + nPixels;
end;

procedure TMainForm.InitNails(ACount: Integer);
var
  diam: Integer;
begin
  if FWorkImg = nil then
    exit;

  if FWorkImg.Width < FWorkImg.Height then
    diam := FWorkImg.Width
  else
    diam := FWorkImg.Height;

  FUsedNails.Clear;
  InitNails(ACount, diam, FNailPos);
end;

procedure TMainForm.InitNails(ACount: Integer; ADiameter: Double;
  var ANailPos: TDoublePointArray);
var
  R: Double;
  C: TDoublePoint;
  phi0, dphi: Double;
  sinPhi, cosPhi: Double;
  i: Integer;
begin
  C.X := ADiameter * 0.5;
  C.Y := C.X;
  R := C.X - NAIL_RADIUS;
  dphi := 2*pi / ACount;
  phi0 := 0.0;
  SetLength(ANailPos, ACount);
  for i := 0 to ACount - 1 do
  begin
    SinCos(phi0 - i*dPhi, sinPhi, cosPhi);      // "-" for CCW orientation
    ANailPos[i].X := R * cosPhi + C.X;
    ANailPos[i].Y := R * sinPhi + C.Y;
  end;
end;

procedure TMainForm.LoadImage(const AFileName: String);
begin
  FWorkCanvas.Free;
  FWorkImg.Free;
  FOrigImg.Free;

  OrigImage.Picture.LoadFromFile(AFileName);
  FOrigImg := OrigImage.Picture.Bitmap.CreateIntfImage;
  FWorkImg := OrigImage.Picture.Bitmap.CreateIntfImage;
  lblImgSize.Caption := Format('Image size: %d x %d', [FOrigImg.Width, FOrigImg.Height]);

  FPaintboxMargin := TextSize('9999', Paintbox.Font) + Size(NAIL_RADIUS, NAIL_RADIUS);
  Paintbox.Width := FOrigImg.Width + 2 * FPaintboxMargin.CX;
  Paintbox.Height := FOrigImg.Height + 2 * FPaintboxMargin.CY;

  if rbGrayScale.Checked then
    MakeGrayScale;
  if rbMonochrome.Checked then
    MakeMonochrome;

  FWorkCanvas := TLazCanvas.Create(FWorkImg);

  InitNails(seNumNails.Value);
  AddToFileHistory(AFileName);
  lblMax.Caption := Format('Max line count: %.0n', [NumNails * (NumNails-1) / 2]);

  Reset;
end;

procedure TMainForm.MakeGrayScale;
var
  x,y: Integer;
  c: TColor;
begin
  if FWorkImg = nil then
    exit;

  FWorkImg.CopyPixels(FOrigImg);
  for y := 0 to FWorkImg.Height-1 do
    for x := 0 to FWorkImg.Width - 1 do
    begin
      c := ColorToGray(FPColorToTColor(FWorkImg.Colors[x, y]));
      FWorkImg.Colors[x, y] := FPColor($101*c, $101*c, $101*c);
    end;
  Paintbox.Invalidate;
end;

procedure TMainForm.MakeMonochrome;
var
  x,y: Integer;
  c: TColor;
  limit: Integer;
begin
  if FWorkImg = nil then
    exit;

  FWorkImg.CopyPixels(FOrigImg);
  limit := TrackBar.Position;
  for y := 0 to FWorkImg.Height-1 do
    for x := 0 to FWorkImg.Width - 1 do
    begin
      c := ColorToGray(FPColorToTColor(FWorkImg.Colors[x, y]));
      FWorkImg.Colors[x, y] := TColorToFPColor(c);
      if c < limit then
        FWorkImg.Colors[x, y] := colGray
      else
        FWorkImg.Colors[x, y] := colWhite;
    end;
  Paintbox.Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    if cgDisplay.Checked[SHOW_ORIG_IMAGE] and (FOrigImg <> nil) then
    begin
      bmp.LoadFromIntfImage(FOrigImg);
      Paintbox.Canvas.Draw(FPaintboxMargin.CX, FPaintboxMargin.CY, bmp);
    end else
    if cgDisplay.Checked[SHOW_WORK_IMAGE] and (FWorkImg <> nil) then
    begin
      bmp.LoadFromIntfImage(FWorkImg);
      Paintbox.Canvas.Draw(FPaintboxMargin.CX, FPaintboxMargin.CY, bmp);
    end else
    begin
      Paintbox.Canvas.Brush.Color := clDefault;
      Paintbox.Canvas.FillRect(0, 0, Paintbox.Width, Paintbox.Height);
    end;
  finally
    bmp.Free;
  end;

  if cgDisplay.CHECKED[SHOW_STRING_IMAGE] then
    DrawStringImg(Paintbox.Canvas, FNailPos, TPoint(FPaintboxMargin));

  if cgDisplay.Checked[SHOW_NAILS] then
    DrawNails(TPoint(FPaintboxMargin));
end;

function TMainForm.NextNail(ANail: Integer; ADelta: Integer = 1): Integer;
begin
  Result := (ANail + ADelta) mod NumNails;
end;

function TMainForm.NumNails: Integer;
begin
  Result := Length(FNailPos);
end;

procedure TMainForm.Process(ANumLines: Integer);
var
  nail1, nail2: Integer;
  P1, P2: TPoint;
  i: Integer;
begin
  Screen.BeginWaitCursor;
  try
    FWorkCanvas.Pen.FPColor := colWhite;
    FWorkCanvas.Pen.Style := psSolid;

    nail1 := FLastNail;
    for i := 0 to ANumLines - 1 do
    begin
      nail2 := NextNail(nail1);
      FindDarkestLine(nail1, nail2);
      {%H-}FUsedNails.Add(nail2);
      P1 := FNailPos[nail1].Round;
      P2 := FNailPos[nail2].Round;
      FWorkCanvas.Line(P1, P2);
      FLastNail := nail2;
      nail1 := nail2;
    end;
    Paintbox.Invalidate;
    btnSave.Enabled := FUsedNails.Count > 1;
    btnSaveConnections.Enabled := btnSave.Enabled;

    UpdateTotalLineCount;
    UpdateResultsPage;

  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.rbGrayscaleChange(Sender: TObject);
begin
  Reset;
  MakeGrayscale;
  Trackbar.Enabled := false;
  Paintbox.Invalidate;
end;

procedure TMainForm.rbMonochromeChange(Sender: TObject);
begin
  Reset;
  MakeMonochrome;
  Trackbar.Enabled := true;
  Paintbox.Invalidate;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L: TStrings;
  i, n: Integer;
  s: String;
  b1, b2: Boolean;
begin
  ini := CreateIni;
  try
    n := ini.ReadInteger('Parameters', 'NumNails', seNumNails.Value);
    if n > 0 then
      seNumNails.Value := n;

    b1 := ini.ReadBool('Parameters', 'ConvertToGrayScale', true);
    b2 := ini.ReadBool('Parameters', 'ConvertToMonochrome', false);
    if b1 then
      rbGrayScale.Checked := true
    else
    if b2 then
      rbMonochrome.Checked := true;

    Trackbar.Position := ini.ReadInteger('Parameters', 'MonochromeThreshold', Trackbar.Position);

    cgDisplay.Checked[SHOW_STRING_IMAGE] := ini.ReadBool('Parameters', 'Display StringImage', true);
    cgDisplay.Checked[SHOW_NAILS]        := ini.ReadBool('Parameters', 'Display Nails', false);
    cgDisplay.Checked[SHOW_ORIG_IMAGE]   := ini.ReadBool('Parameters', 'Display OrigImage', false);
    cgDisplay.Checked[SHOW_WORK_IMAGE]   := ini.ReadBool('Parameters', 'Display WorkImage', false);

    n := ini.ReadInteger('Parameters', 'NumLines', seNumLines.Value);
    if n > 0 then
      seNumLines.Value := n;

    L := TStringList.Create;
    try
      ini.ReadSection('RecentlyUsed', L);
      cbFileNames.Items.Clear;
      n := Min(L.Count, MAX_HISTORY);
      for i := 0 to n-1 do
      begin
        s := ini.ReadString('RecentlyUsed', L[i], '');
        if (s <> '') and FileExists(s) and (cbFileNames.Items.IndexOf(s) = -1) then
          cbFileNames.Items.Add(s);
      end;
    finally
      L.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.Reset;
begin
  InitNails(seNumNails.Value);
  if (FWorkImg <> nil) and (FOrigImg <> nil) then
  begin
    FWorkImg.CopyPixels(FOrigImg);
    if rbGrayScale.Checked then MakeGrayscale else
    if rbMonochrome.Checked then MakeMonochrome;
  end;
  FLastNail := 0;
  FUsedNails.Clear;
  {%H-}FUsedNails.Add(FLastNail);
  btnSave.Enabled := false;
  btnSaveConnections.Enabled := false;
  UpdateTotalLineCount;
  UpdateResultsPage;
  Paintbox.Invalidate;
end;

procedure TMainForm.SaveConnections(const AFileName: String);
var
  F: TextFile;
  i: Integer;
begin
  System.Assign(F, AFileName);
  Rewrite(F);
  for i := 0 to FUsedNails.Count-1 do
    WriteLn(F, 'Index ', i, ': Nail #', FUsedNails[i]);
  CloseFile(F);
end;

procedure TMainForm.SaveImage(const AFileName: String);
const
  FACTOR = 2.0;
var
  img: TCustomBitmap;
  nailPos: TDoublePointArray = nil;
begin
  img := TPortableNetworkGraphic.Create;
  try
    img.SetSize(round(FOrigImg.Width * FACTOR), round(FOrigImg.Height * FACTOR));
    img.Canvas.Brush.Color := clWhite;
    img.Canvas.FillRect(0, 0, img.Width, img.Height);
    InitNails(seNumNails.Value, FOrigImg.Width * FACTOR, nailPos);
    DrawStringImg(img.Canvas, nailPos, Point(0, 0));
    img.SaveToFile(AFileName);
  finally
    img.Free;
  end;
end;

procedure TMainForm.seImgDiameterChange(Sender: TObject);
begin
  UpdateWireLength;
end;

procedure TMainForm.seNumNailsChange(Sender: TObject);
var
  n: Integer;
begin
  InitNails(seNumNails.Value);
  n := seNumNails.Value;
  lblMax.Caption := Format('Max line count: %.0n', [n * (n-1) / 2]);
  Paintbox.Invalidate;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  if rbMonochrome.Checked then
    MakeMonochrome;
end;

procedure TMainForm.UpdateResultsPage;
var
  i: Integer;
begin
  UpdateWireLength;

  if (FWorkImg = nil) or (FUsedNails.Count <= 1) then
    lbUsedNails.Items.Clear
  else
  begin
    lbUsedNails.Items.BeginUpdate;
    try
      lbUsedNails.Items.Clear;
      for i := 0 to FUsedNails.Count-1 do
        lbUsedNails.Items.Add(Format('Index %d: Nail #%d', [i, FUsedNails[i]]));
    finally
      lbUsedNails.Items.EndUpdate;
    end;
  end;
end;

procedure TMainForm.UpdateTotalLineCount;
begin
  lblTotalLineCount.Caption := Format('Total line count: %d', [FUsedNails.Count-1]);
end;

procedure TMainForm.UpdateWirelength;
const
  TOTAL_LENGTH_TEXT = 'Total length of wire: ';

  function Distance(P1, P2: TDoublePoint): Double;
  begin
    Result := sqrt(sqr(P2.X - P1.X) + sqr(P2.Y - P1.Y));
  end;

var
  i: Integer;
  len: Double;
  R: Double;
  P1, P2: TDoublePoint;
begin
  if (FWorkImg = nil) or (FUsedNails.Count <= 1) then
  begin
    lblLength.Caption := TOTAL_LENGTH_TEXT;
    exit;
  end;

  R := seImgDiameter.Value * 0.01 / 2;  // "real" image radius, in meters
  len := 0;
  P1 := FNailPos[0];
  for i := 1 to FUsedNails.Count-1 do
  begin
    lbUsedNails.Items.Add(Format('Index %d: Nail #%d', [i, FUsedNails[i]]));
    P2 := FNailPos[FUsedNails[i]];
    len := len + Distance(P1, P2);
    P1 := P2;
  end;
  len := len / (FWorkImg.Width*0.5) * R;
  LblLength.Caption := Format(TOTAL_LENGTH_TEXT + '%.2n m', [len]);
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := CreateIni;
  try
    ini.EraseSection('Parameters');
    ini.WriteInteger('Parameters', 'NumNails', seNumNails.Value);
    ini.WriteBool('Parameters', 'ConvertToGrayScale', rbGrayscale.Checked);
    ini.WriteBool('Parameters', 'ConvertToMonochrome', rbMonochrome.Checked);
    ini.WriteInteger('Parameters', 'MonochromeThreshold', Trackbar.Position);
    ini.WriteBool('Parameters', 'Display StringImage', cgDisplay.Checked[SHOW_STRING_IMAGE]);
    ini.WriteBool('Parameters', 'Display Nails', cgDisplay.Checked[SHOW_NAILS]);
    ini.WriteBool('Parameters', 'Display OrigImage', cgDisplay.Checked[SHOW_ORIG_IMAGE]);
    ini.WriteBool('Parameters', 'Display WorkImage', cgDisplay.Checked[SHOW_WORK_IMAGE]);
    ini.WriteInteger('Parameters', 'NumLines', seNumLines.Value);

    ini.EraseSection('RecentlyUsed');
    for i := 0 to cbFileNames.Items.Count-1 do
      ini.WriteString('RecentlyUsed', 'File' + IntToStr(i+1), cbFileNames.Items[i]);
  finally
    ini.Free;
  end;
end;

end.

