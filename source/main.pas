unit Main;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, IniFiles,
  LCLIntf, LCLType, Graphics, GraphUtil, Types,
  Forms, Controls, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, Buttons, ExtDlgs,
  Grids, mcGrid,
  FPImage, IntfGraphics, LazCanvas;

type
  TDoublePoint = record
    X, Y: Double;
    function Round: TPoint;
  end;
  TDoublePointArray = array of TDoublePoint;

  TOperation = (opLineTo, opMoveTo);

  TUsedNail = record
    Index: Integer;
    Operation: TOperation;
    class operator = (N1, N2: TUsedNail): Boolean;
    class operator <> (N1, N2: TUsedNail): Boolean;
  end;

  TUsedNailList = class(specialize TFPGList<TUsedNail>)
  public
    function Add(AIndex: Integer): Integer;
  end;

type
  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnReset: TButton;
    btnCalculate: TButton;
    btnSaveImage: TButton;
    btnSaveConnections: TButton;
    btnSaveHardwareImage: TButton;
    cbFileNames: TComboBox;
    cgDisplay: TCheckGroup;
    fseBrightnessTransferExponent: TFloatSpinEdit;
    EditImage: TImage;
    infoNailDistance: TLabel;
    infoWirelength: TLabel;
    lblEditConnectionsHelp: TLabel;
    lblMillimeters3: TLabel;
    lblPixels: TLabel;
    lblRealLineWidth: TLabel;
    lblLineWidth: TLabel;
    lblMillimeters2: TLabel;
    lblNailDistance: TLabel;
    lblBrightnessTransferExponent: TLabel;
    lblMillimeters1: TLabel;
    lblWireLength: TLabel;
    ConnectionsGridPanel: TPanel;
    ProgressBar: TProgressBar;
    seImgDiameter: TFloatSpinEdit;
    gbImgSelection: TGroupBox;
    gbImgPreparation: TGroupBox;
    gbProcess: TGroupBox;
    lblNumLines: TLabel;
    lblImgDiameter: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    OrigImage: TImage;
    lblFileName: TLabel;
    lblImgSize: TLabel;
    lblNumNails: TLabel;
    PageControl: TPageControl;
    PaintBox: TPaintBox;
    ParamsPanel: TPanel;
    ConnectionsHeaderPanel: TPanel;
    rbGrayscale: TRadioButton;
    rbMonochrome: TRadioButton;
    SaveDialog: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox: TScrollBox;
    seRealLineWidth: TFloatSpinEdit;
    seNumNails: TSpinEdit;
    seNumLines: TSpinEdit;
    btnBrowse: TSpeedButton;
    btnOpen: TSpeedButton;
    pgImages: TTabSheet;
    pgConnections: TTabSheet;
    seLineWidth: TSpinEdit;
    StatusBar: TStatusBar;
    tbEditConnections: TToggleBox;
    TrackBar: TTrackBar;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnSaveConnectionsClick(Sender: TObject);
    procedure btnSaveHardwareImageClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure cbFileNamesChange(Sender: TObject);
    procedure cbFileNamesCloseUp(Sender: TObject);
    procedure cbFileNamesDropDown(Sender: TObject);
    procedure cbShowOrigImgChange(Sender: TObject);
    procedure cgDisplayItemClick(Sender: TObject; {%H-}Index: integer);
    procedure cgIncludeInListItemClick(Sender: TObject; {%H-}Index: integer);
    procedure EditImageResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure rbGrayscaleChange(Sender: TObject);
    procedure rbMonochromeChange(Sender: TObject);
    procedure seImgDiameterChange(Sender: TObject);
    procedure seNumNailsChange(Sender: TObject);
    procedure tbEditConnectionsChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FOrigImg: TLazIntfImage;
    FWorkImg: TLazIntfImage;
    FWorkCanvas: TLazCanvas;
    FNailPos: array of TDoublePoint;
    FUsedNails: TUsedNailList;
    FLastNailIndex: Integer;
    FPaintboxMargin: TSize;
    FConnectionGrid: TMCStringGrid;
    FActivated: Boolean;
    FAborted: Boolean;
    FCalculating: Boolean;
    FEditing: Boolean;

    function GetAverageLineGray(AImage: TFPCustomImage; ANail1, ANail2, ALineWidth: Integer): Double;
    procedure GetThickLineGray(AImage: TFPCustomImage;
      x1, y1, x2, y2, ALineWidth: Integer; var ASum: Double; var ACount: Integer);
    procedure GetThinLineGray(AImage: TFPCustomImage;
      x1, y1, x2, y2: Integer; var ASum: Double; var ACount: Integer);

  protected
    procedure AddToFileHistory(const AFileName: String);
    procedure DrawEditImage(ARow: Integer);
    procedure DrawNails(AOffset: TPoint);
    procedure DrawStringImg(ACanvas: TCanvas; const ANailPos: TDoublePointArray; AOffset: TPoint; ALineWidth: Integer);
    procedure EnableDisableControls(AEnable: Boolean);
    function FindDarkestLine(AImage: TFPCustomImage; ANail1: Integer; var ANail2: Integer): Boolean;
    procedure GridKeyDownHandler(Sender: TObject; var Key : Word; Shift : TShiftState);
    procedure GridMergeCellsHandler(Sender: TObject; ACol, ARow: Integer; var ALeft, ATop, ARight, ABottom: Integer);
    procedure GridPrepareCanvasHandler(Sender: TObject; {%H-}ACol, {%H-}ARow: Integer; {%H-}AState: TGridDrawState);
    procedure GridSelectCellHandler(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure InitNails(ACount: Integer);
    procedure InitNails(ACount: Integer; ADiameter: Double; var ANailPos: TDoublePointArray);
    procedure LoadImage(const AFileName: String);
    procedure MakeGrayScale(AImage: TFPCustomImage);
    procedure MakeMonochrome(AImage: TFPCustomImage);
    procedure MakeWorkImage(AImage: TFPCustomImage);
    function NextNail(ANail: Integer; ADelta: Integer = 1): Integer;
    function NumNails: Integer;
    procedure Process(ANumLines: Integer);
    procedure Reset;
    procedure SaveConnections(const AFileName: String);
    procedure SaveImage(const AFileName: String; ASize: Double = -1);
    procedure UpdateBtnStates;
    procedure UpdateCaption;
    procedure UpdateConnectionList;
    procedure UpdateMaxLineCount;
    procedure UpdateNailDistance;
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
  MAIN_CAPTION = 'STRING ART';
  NAIL_RADIUS = 2;
  MAX_HISTORY = 20;

  SHOW_STRING_IMAGE = 0;
  SHOW_NAILS = 1;
  SHOW_ORIG_IMAGE = 2;
  SHOW_WORK_IMAGE = 3;

type
  TBrightnessTransferFunction = function(x, y: Double): Double;

function LinearTransferFunction(x, {%H-}y: double): Double;
begin
  Result := x;
end;

function SqrtTransferFunction(x, {%H-}y: Double): Double;
begin
  Result := sqrt(x);
end;

function PowerTransferFunction(x,y : Double): Double;
begin
  Result := Power(x, y);
end;

function TDoublePoint.Round: TPoint;
begin
  Result := Point(System.Round(Self.X), System.Round(Self.Y));
end;

function CreateIni: TCustomIniFile;
begin
  Result := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

function mmToPx(mm: Double; PixelsPerInch: Integer): Integer;
begin
  Result := round(mm/25.4 * PixelsPerInch);
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

{ Set font style of Groupbox caption to bold, but keep items normal }
procedure BoldGroup(AControl: TCustomGroupBox);
var
  i: Integer;
begin
  AControl.Font.Style := [fsBold];
  for i:=0 to AControl.ControlCount-1 do
    AControl.Controls[i].Font.Style := [];
end;

procedure BoldTabs(AControl: TPageControl);
var
  i: Integer;
begin
  AControl.Font.Style := [fsBold];
  for i:=0 to AControl.ControlCount-1 do
    AControl.Controls[i].Font.Style := [];
end;


{ TUsedNail }

class operator TUsedNail.=(N1, N2: TUsedNail): Boolean;
begin
  Result := N1.Index = N2.Index;
end;

class operator TUsedNail.<>(N1, N2: TUsedNail): Boolean;
begin
  Result := N1.Index <> N2.Index;
end;


{ TUsedNailList }

function TUsedNailList.Add(AIndex: Integer): Integer;
var
  nail: TUsedNail;
begin
  nail.Index := AIndex;
  nail.Operation := opLineTo;
  Result := inherited Add(nail);
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
  if (cbFilenames.Text <> '') then
    LoadImage(cbFileNames.Text);
end;

procedure TMainForm.btnCalculateClick(Sender: TObject);
begin
  if FCalculating then
    FAborted := true
  else
  begin
    FAborted := false;
    Process(seNumLines.Value);
  end;
end;

procedure TMainForm.btnSaveConnectionsClick(Sender: TObject);
begin
  with SaveDialog do
  begin
    if FileName <> '' then
    begin
      InitialDir := ExtractFileDir(FileName);
      FileName := '';
    end;
    if Execute then
      SaveConnections(FileName);
  end;
end;

procedure TMainForm.btnSaveHardwareImageClick(Sender: TObject);
begin
  if FOrigImg = nil then
    exit;

  with SavePictureDialog do
  begin
    if FileName <> '' then
    begin
      InitialDir := ExtractFileDir(FileName);
      FileName := '';
    end;
    if Execute then
      SaveImage(FileName, seImgDiameter.Value);
  end;
end;

procedure TMainForm.btnSaveImageClick(Sender: TObject);
begin
  if FOrigImg = nil then
    exit;

  with SavePictureDialog do
  begin
    if FileName <> '' then
    begin
      InitialDir := ExtractFileDir(FileName);
      FileName := '';
    end;
    if Execute then
      SaveImage(FileName);
  end;
end;

procedure TMainForm.cbFileNamesChange(Sender: TObject);
begin
  btnOpen.Enabled := (cbFileNames.Text <> '');
end;

procedure TMainForm.cbFileNamesCloseUp(Sender: TObject);
begin
  if cbFileNames.ItemIndex > -1 then
  begin
    cbFileNames.Text := cbFileNames.Items[cbFileNames.ItemIndex];
    LoadImage(cbFileNames.Text);
  end;
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

procedure TMainForm.cgIncludeInListItemClick(Sender: TObject; Index: integer);
begin
  UpdateConnectionList;
end;

procedure TMainForm.EditImageResize(Sender: TObject);
begin
  if FEditing then
    DrawEditImage(FConnectionGrid.Row);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    Constraints.MinWidth := lblMillimeters1.Left + lblMillimeters1.Width +
      btnSaveHardwareImage.Width + btnSaveConnections.Width + 3*btnSaveConnections.BorderSpacing.Left +
      2*ConnectionsHeaderPanel.BorderSpacing.Around + PageControl.Left;
    if Width < Constraints.MinWidth then
      Width := 0;
    Constraints.MinHeight := gbProcess.Top + gbProcess.Height +
      2*ParamsPanel.BorderSpacing.Top + StatusBar.Height;
    if Height < Constraints.MinHeight then
      Height := 0;
    FActivated := true;
  end;
end;

procedure TMainForm.DrawEditImage(ARow: Integer);
var
  bmp: TBitmap;
  nailPos: TDoublePointArray = nil;
  w, h, wImg, linewidth: Integer;
  dx, dy: Integer;
  idx: Integer;
  P: TPoint;
begin
  if FUsedNails.Count = 0 then
    exit;

  w := EditImage.Width;
  h := EditImage.Height;
  lineWidth := 1;
  bmp := TBitmap.Create;
  try
    bmp.SetSize(w, h);
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    wImg := Min(w, h);
    InitNails(seNumNails.Value, wImg, nailPos);

    dx := (w - wImg) div 2;
    dy := (h - wImg) div 2;
    DrawStringImg(bmp.Canvas, nailPos, Point(dx, dy), linewidth);

    if (ARow >= FConnectionGrid.FixedRows) then
    begin
      idx := ARow - FConnectionGrid.FixedRows + 1;
      if FUsedNails[idx].Operation = opMoveTo then
        bmp.Canvas.Pen.Color := clMaroon
      else
        bmp.Canvas.Pen.Color := clRed;
      bmp.Canvas.Pen.Width := 3;
      P := nailPos[FUsedNails[idx-1].Index].Round + Point(dx, dy);
      bmp.Canvas.MoveTo(P);
      P := nailPos[FUsedNails[idx].Index].Round + Point(dx, dy);
      bmp.Canvas.LineTo(P);
    end;

    EditImage.Picture.Bitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.DrawStringImg(ACanvas: TCanvas;
  const ANailPos: TDoublePointArray; AOffset: TPoint; ALineWidth: Integer);
var
  i: Integer;
  P: TPoint;
  nail: TUsedNail;
begin
  if FUsedNails.Count <= 1 then
    exit;

  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := ALineWidth;
  P := ANailPos[FUsedNails{%H-}[0].Index].Round + AOffset;
  ACanvas.MoveTo(P);
  for i := 1 to FUsedNails.Count-1 do
  begin
    nail := FUsedNails[i];
    P := ANailPos[nail.Index].Round + AOffset;
    case nail.Operation of
      opMoveTo: ACanvas.MoveTo(P);
      opLineTo: ACanvas.LineTo(P);
    end;
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
      Paintbox.Canvas.TextOut(x, y, s);
    end;
  end;
end;

procedure TMainForm.EnableDisableControls(AEnable: Boolean);
begin
  cbFilenames.Enabled := AEnable;
  lblFileName.Enabled := AEnable;
  btnBrowse.Enabled := AEnable;
  btnOpen.Enabled := AEnable;

  seNumNails.Enabled := AEnable;
  lblNumNails.Enabled := AEnable;

  seLineWidth.Enabled := AEnable;
  lblLineWidth.Enabled := AEnable;
  lblPixels.Enabled := AEnable;

  rbGrayscale.Enabled := AEnable;
  rbMonochrome.Enabled := AEnable;
  Trackbar.Enabled := AEnable and rbMonochrome.Checked;

  seNumLines.Enabled := AEnable;
  lblNumLines.Enabled := AEnable;
  fseBrightnessTransferExponent.Enabled := AEnable;
  lblBrightnessTransferExponent.Enabled := AEnable;

  seImgDiameter.Enabled := AEnable;
  lblImgDiameter.Enabled := AEnable;
  lblMillimeters1.Enabled := AEnable;

  seRealLineWidth.Enabled := AEnable;
  lblRealLineWidth.Enabled := AEnable;
  lblMillimeters2.Enabled := AEnable;

  btnReset.Enabled := AEnable;
  btnSaveImage.Enabled := AEnable and (FUsedNails.Count > 1);
  btnSaveHardwareImage.Enabled := btnSaveImage.Enabled;
  btnSaveConnections.Enabled := btnSaveImage.Enabled;
end;

function TMainForm.FindDarkestLine(AImage: TFPCustomImage;
  ANail1: Integer; var ANail2: Integer): Boolean;
var
  nail2: Integer;
  nailIndexForDarkestLine, lineWidth: Integer;
  gray, darkestGray: Double;
  i: Integer;
begin
  lineWidth := seLineWidth.Value;
  nailIndexForDarkestLine := -1;
  darkestGray := MaxDouble;
  for i := 0 to High(FNailPos) do
  begin
    nail2 := NextNail(ANail2, i);
    if nail2 = ANail1 then
      continue;
    gray := GetAverageLineGray(AImage, ANail1, nail2, lineWidth);
    if gray < darkestGray then
    begin
      darkestGray := gray;
      nailIndexForDarkestLine := nail2;
    end;
  end;
  ANail2 := nailIndexForDarkestLine;
  Result := (nailIndexForDarkestLine > -1) and (darkestGray < 1.000);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FUsedNails := TUsedNailList.Create;

  Caption := MAIN_CAPTION;
  PageControl.TabIndex := 0;
  cgDisplay.Checked[SHOW_STRING_IMAGE] := true;

  BoldTabs(PageControl);
  BoldGroup(gbImgSelection);
  BoldGroup(gbImgPreparation);
  BoldGroup(cgDisplay);
  BoldGroup(gbProcess);
  btnCalculate.Font.Style := [fsBold];

  Paintbox.Width := 0;
  Paintbox.Height := 0;

  FConnectionGrid := TMCStringGrid.Create(self);
  FConnectionGrid.Parent := ConnectionsGridPanel;
  FConnectionGrid.Align := alClient;
  FConnectionGrid.MouseWheelOption := mwGrid;
  FConnectionGrid.Options := FConnectionGrid.Options + [goColSpanning, goThumbTracking, goRowSelect];
  FConnectionGrid.TitleStyle := tsNative;
  FConnectionGrid.OnKeyDown := @GridKeyDownHandler;
  FConnectionGrid.OnPrepareCanvas := @GridPrepareCanvasHandler;
  FConnectionGrid.OnMergeCells := @GridMergeCellsHandler;
  FConnectionGrid.OnSelectCell := @GridSelectCellHandler;
  FConnectionGrid.ColCount := 8;
  FConnectionGrid.FixedRows := 2;
  FConnectionGrid.Cells[0, 0] := 'Line' + LineEnding + 'No.';
  FConnectionGrid.Cells[1, 0] := 'From';
  FConnectionGrid.Cells[4, 0] := 'To';
  FConnectionGrid.Cells[7, 0] := 'Gray' + LineEnding + '(0...1)';
  FConnectionGrid.Cells[1, 1] := 'Nail No.';
  FConnectionGrid.Cells[2, 1] := 'x (mm)';
  FConnectionGrid.Cells[3, 1] := 'y (mm)';
  FConnectionGrid.Cells[4, 1] := 'Nail No.';
  FConnectionGrid.Cells[5, 1] := 'x (mm)';
  FConnectionGrid.Cells[6, 1] := 'y (mm)';

  ReadIni;
  if ParamCount > 0 then
    cbFileNames.Text := ParamStr(1);

  if (cbFileNames.Text <> '') and FileExists(cbFileNames.Text) then
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

function TMainForm.GetAverageLineGray(AImage: TFPCustomImage;
  ANail1, ANail2, ALineWidth: Integer): Double;
var
  P1, P2: TPoint;
  count: Integer;
  sum: Double;
begin
  if (ANail1 < 0) or (ANail1 > High(FNailPos)) or
     (ANail2 < 0) or (ANail2 > High(FNailPos)) then
  begin
    Result := MaxInt;
    exit;
  end;

  P1 := FNailPos[ANail1].Round;
  P2 := FNailPos[ANail2].Round;

  sum := 0.0;
  count := 0;
  if ALineWidth = 1 then
    GetThinLineGray(AImage, P1.X, P1.Y, P2.X, P2.Y, sum, count)
  else
    GetThickLineGray(AImage, P1.X, P1.Y, P2.X, P2.Y, ALineWidth, sum, count);

  Result := sum / count;
end;


procedure TMainForm.GetThickLineGray(AImage: TFPCustomImage;
  x1, y1, x2, y2, ALineWidth: Integer;
  var ASum: Double; var ACount: Integer);
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
      GetThinLineGray(AImage, x1-r, y1, x2-r, y2, ASum, ACount);
    for r := 1 to w2 do
      GetThinLineGray(AImage, x1+r, y1, x2+r, y2, ASum, ACount);
  end else
  begin  // add lines above/under
    for r := 1 to w1 do
      GetThinLineGray(AImage, x1, y1-r, x2, y2-r, ASum, ACount);
    for r := 1 to w2 do
      GetThinLineGray(AImage, x1, y1+r, x2, y2+r, ASum, ACount);
  end;
end;

procedure TMainForm.GetThinLineGray(AImage: TFPCustomImage;
  x1, y1, x2, y2: Integer; var ASum: Double; var ACount: Integer);

  function GetPixelGray(x, y: Integer): Double;
  begin
    if (x < 0) or (x >= AImage.Width) or (y < 0) or (y >= AImage.Height) then
      Result := 1.0
    else
      Result := AImage.Colors[x, y].Red / word($FFFF);  // We have grayscale, i.e. Red is ok
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
  for i := 0 to {%H-}nPixels-1 do
  begin
    ASum := ASum + GetPixelGray(x, y);
    if {%H-}d < 0 then
    begin
      d := d + {%H-}dinc1;
      x := x + {%H-}xinc1;
      y := y + {%H-}yinc1;
    end else
    begin
      d := d + {%H-}dinc2;
      x := x + {%H-}xinc2;
      y := y + {%H-}yinc2;
    end;
  end;
  ACount := ACount + nPixels;
end;

procedure TMainForm.GridKeyDownHandler(Sender: TObject; var Key : Word; Shift : TShiftState);
var
  idx: Integer;
  P: TPoint;
  nail: TUsedNail;
  s: String;
begin
  if Key = VK_DELETE then
  begin
    // Find endpoint of row to be deleted
    idx := FConnectionGrid.Row - FConnectionGrid.FixedRows + 1;
    s := FConnectionGrid.Cells[0, FConnectionGrid.Row];
    // For deletion, mark the endpoint as a "moveto" point (in contrast to others which are "lineto")
    // A second DEL press undeletes the line.
    // Delete rows are also marked by a * in the counter column.
    nail := FUsedNails[idx];
    if nail.Operation = opLineTo then
    begin
      nail.Operation := opMoveTo;
      if s[1] <> '*' then s := '*' + s;
    end else
    begin
      nail.Operation := opLineTo;
      if s[1] = '*' then Delete(s, 1, 1);
    end;
    FUsedNails[idx] := nail;
    FConnectionGrid.Cells[0, FConnectionGrid.Row] := s;
    // Draw the modified string image.
    DrawEditImage(FConnectionGrid.Row);
    Key := 0;
  end;
  inherited;
end;

procedure TMainForm.GridMergeCellsHandler(Sender: TObject; ACol, ARow: Integer;
  var ALeft, ATop, ARight, ABottom: Integer);
begin
  if (ARow in [0, 1]) and ((ACol = 0) or (ACol = 7)) then
  begin
    ATop := 0;
    ABottom := 1;
  end else
  if (ACol in [1..3]) and (ARow = 0) then
  begin
    ALeft := 1;
    ARight := 3;
  end else
  if (ACol in [4..6]) and (ARow = 0) then
  begin
    ALeft := 4;
    ARight := 6;
  end;
end;

procedure TMainForm.GridPrepareCanvasHandler(Sender: TObject; ACol, ARow: Integer;
  AState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := FConnectionGrid.Canvas.TextStyle;
  ts.Alignment := taCenter;
  ts.SingleLine := false;

  if (ARow >= FConnectionGrid.FixedRows) and
     (FUsedNails[ARow - FConnectionGrid.FixedRows + 1].Operation = opMoveTo)
  then
    FConnectionGrid.Canvas.Font.Color := clSilver;

  FConnectionGrid.Canvas.TextStyle := ts;
end;

procedure TMainForm.GridSelectCellHandler(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  DrawEditImage(ARow);
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
  R := C.X;
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
var
  wh: Integer;
begin
  if not FileExists(AFilename) then
  begin
    MessageDlg(Format('File "%s" not found.', [AFileName]), mtError, [mbOk], 0);
    exit;
  end;

  FWorkCanvas.Free;
  FWorkImg.Free;
  FOrigImg.Free;

  OrigImage.Picture.LoadFromFile(AFileName);
  FOrigImg := OrigImage.Picture.Bitmap.CreateIntfImage;
  if FOrigImg.Width > FOrigImg.Height then
    wh := FOrigImg.Height
  else
    wh := FOrigImg.Width;
  FWorkImg := TLazIntfImage.CreateCompatible(FOrigImg, wh, wh);
  lblImgSize.Caption := Format('Image size: %d x %d', [FOrigImg.Width, FOrigImg.Height]);

  FPaintboxMargin := TextSize('9999', Paintbox.Font) + Size(NAIL_RADIUS, NAIL_RADIUS);
  Paintbox.Width := FOrigImg.Width + 2 * FPaintboxMargin.CX;
  Paintbox.Height := FOrigImg.Height + 2 * FPaintboxMargin.CY;

  MakeWorkImage(FWorkImg);
  FWorkCanvas := TLazCanvas.Create(FWorkImg);

  InitNails(seNumNails.Value);
  AddToFileHistory(AFileName);
  UpdateMaxLineCount;
  UpdateCaption;

  Reset;
end;

procedure TMainForm.MakeGrayScale(AImage: TFPCustomImage);
const
  MAX_GRAY: word = $FFFF;
var
  x,y: Integer;
  grayValue: Word;
  gray: Double;       // 0 ... 1
  BTF: TBrightnessTransferFunction;
  BTFexponent: Double;
begin
  if (AImage = nil) or (FOrigImg = nil) then
    exit;

  BTFExponent := fseBrightnessTransferExponent.Value;
  if BTFExponent = 1.0 then
    BTF := @LinearTransferFunction
  else if BTFExponent = 0.5 then
    BTF := @SqrtTransferFunction
  else
    BTF := @PowerTransferFunction;

  for y := 0 to AImage.Height-1 do
    for x := 0 to AImage.Width - 1 do
    begin
      grayValue := CalculateGray(FOrigImg.Colors[x, y]);
      gray := BTF(grayValue / MAX_GRAY, BTFExponent);
      gray := EnsureRange(gray*MAX_GRAY, 0, MAX_GRAY);
      grayValue := round(gray);
      AImage.Colors[x, y] := FPColor(grayValue, grayValue, grayValue);
    end;
end;

procedure TMainForm.MakeMonochrome(AImage: TFPCustomImage);
var
  x,y: Integer;
  c: TColor;
  limit: Integer;
begin
  if (AImage = nil) or (FOrigImg = nil) then
    exit;

  limit := TrackBar.Position;
  for y := 0 to AImage.Height-1 do
    for x := 0 to AImage.Width - 1 do
    begin
      c := ColorToGray(FPColorToTColor(FOrigImg.Colors[x, y]));
      AImage.Colors[x, y] := TColorToFPColor(c);
      if c < limit then
        AImage.Colors[x, y] := colGray
      else
        AImage.Colors[x, y] := colWhite;
    end;
end;

procedure TMainForm.MakeWorkImage(AImage: TFPCustomImage);
begin
  if rbGrayScale.Checked then
    MakeGrayScale(AImage)
  else
  if rbMonochrome.Checked then
    MakeMonochrome(AImage);
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
      Paintbox.Canvas.Brush.Color := clWindow;
      Paintbox.Canvas.FillRect(0, 0, Paintbox.Width, Paintbox.Height);
    end;
  finally
    bmp.Free;
  end;

  if cgDisplay.CHECKED[SHOW_STRING_IMAGE] then
    DrawStringImg(Paintbox.Canvas, FNailPos, TPoint(FPaintboxMargin), seLineWidth.Value);

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
  nail1, nail2: TUsedNail;
  P1, P2: TPoint;
  i: Integer;
  endReached: Boolean;
begin
  if FWorkImg = nil then
    exit;

//  Screen.Cursor := crHourglass;   // BeginWaitCursor requires Laz 3.0+
  EnableDisableControls(false);
  btnCalculate.Caption := 'Abort';
  FCalculating := True;
  endReached := false;
  try
    Progressbar.Position := 0;
    Progressbar.Max := seNumLines.Value-1;
    Progressbar.Visible := true;

    FWorkCanvas.Pen.FPColor := colWhite;
    FWorkCanvas.Pen.Style := psSolid;
    FWorkCanvas.Pen.Width := seLineWidth.Value;

    nail1.Index := FLastNailIndex;
    for i := 0 to ANumLines - 1 do
    begin
      if i mod 10 = 0 then
      begin
        Progressbar.Position := i;
        Progressbar.Repaint;
        UpdateTotalLineCount;
        Application.ProcessMessages;
        if FAborted then exit;
      end;

      nail2.Operation := opLineTo;
      nail2.Index := NextNail(nail1.Index);
      if not FindDarkestLine(FWorkImg, nail1.Index, nail2.Index) then
        endReached := true;
      FUsedNails.Add(nail2.Index);
      P1 := FNailPos[nail1.Index].Round;
      P2 := FNailPos[nail2.Index].Round;
      FWorkCanvas.Line(P1, P2);
      FLastNailIndex := nail2.Index;
      nail1 := nail2;
      if endReached then
        exit;
    end;
  finally
    FAborted := false;
    FCalculating := false;
    Paintbox.Invalidate;
    UpdateTotalLineCount;
    UpdateResultsPage;
    Progressbar.Hide;
    btnCalculate.Caption := 'Calculate';
    EnableDisableControls(true);
    if endReached then
    begin
      Application.ProcessMessages;
      MessageDlg('More iterations do not lead to further improvement of the image.', mtInformation, [mbOk], 0);
    end;
//    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.rbGrayscaleChange(Sender: TObject);
begin
  Reset;
  MakeGrayscale(FWorkImg);
  Trackbar.Enabled := false;
  Paintbox.Invalidate;
end;

procedure TMainForm.rbMonochromeChange(Sender: TObject);
begin
  Reset;
  MakeMonochrome(FWorkImg);
  Trackbar.Enabled := true;
  Paintbox.Invalidate;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  list: TStrings;
  i, n: Integer;
  x: Double;
  s: String;
  b1, b2: Boolean;
  L, T, W, H: Integer;
  R: TRect;
  fs: TFormatSettings;
begin
  fs := DefaultformatSettings;
  fs.DecimalSeparator := '.';

  ini := CreateIni;
  try
    T := ini.ReadInteger('MainForm', 'Top', Top);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);

    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L + W > R.Right then L := R.Right - W;
    if T + H > R.Bottom then T := R.Bottom - H;
    SetBounds(L, T, W, H);

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

    s := ini.ReadString('Parameters', 'BrightnessTransferExponent', '');
    if TryStrToFloat(s, x,fs ) then
      fseBrightnessTransferExponent.Value := x;

    n := ini.ReadInteger('Parameters', 'NumLines', seNumLines.Value);
    if n > 0 then
      seNumLines.Value := n;

    n := ini.ReadInteger('Parameters', 'LineWidth', seLineWidth.Value);
    if n > 0 then
      seLineWidth.Value := n;

    s := ini.ReadString('Parameters', 'RealLineWidth', '');
    if TryStrToFloat(s, x, fs) then
      seRealLineWidth.Value := x;

    s := ini.ReadString('Parameters', 'ImageDiameter', '');
    if TryStrToFloat(s, x, fs) then
      seImgDiameter.Value := x;

    list := TStringList.Create;
    try
      ini.ReadSection('RecentlyUsed', list);
      cbFileNames.Items.Clear;
      n := Min(list.Count, MAX_HISTORY);
      for i := 0 to n-1 do
      begin
        s := ini.ReadString('RecentlyUsed', list[i], '');
        if (s <> '') and FileExists(s) and (cbFileNames.Items.IndexOf(s) = -1) then
          cbFileNames.Items.Add(s);
      end;
    finally
      list.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.Reset;
begin
  InitNails(seNumNails.Value);
  MakeWorkImage(FWorkImg);
  FLastNailIndex := 0;
  FUsedNails.Clear;
  {%H-}FUsedNails.Add(FLastNailIndex);
  UpdateBtnStates;
  UpdateTotalLineCount;
  UpdateResultsPage;
  Paintbox.Invalidate;
end;

procedure TMainForm.SaveConnections(const AFileName: String);
const
  SEPARATOR = #9;
var
  stream: TStream;
  L: TStrings;
  s: String;
  i, j: Integer;
begin
  stream := TFileStream.Create(AFileName, fmCreate or fmShareDenyNone);
  L := TStringList.Create;
  try
    L.Delimiter := SEPARATOR;
    L.StrictDelimiter := true;
    s := 'Line' + SEPARATOR +
         'From Nail' + SEPARATOR + 'x' + SEPARATOR + 'y' + SEPARATOR +
         'To Nail'   + SEPARATOR + 'x' + SEPARATOR + 'y' + SEPARATOR +
         'Gray' + LineEnding;
    stream.Write(s[1], Length(s));
    for i := FConnectionGrid.FixedRows to FConnectionGrid.RowCount - 1 do
    begin
      // Deleted connection
      if (FUsedNails[i - FConnectionGrid.FixedRows + 1].Operation = opMoveTo) then
        continue;
      // Existing connections
      s := FConnectionGrid.Cells[0, i];
      for j := 1 to FConnectionGrid.ColCount-1 do
        s := s + SEPARATOR + StringReplace(FConnectionGrid.Cells[j, i], FormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
      s := s + LineEnding;
      stream.Write(s[1],Length(s));
    end;
  finally
    L.Free;
    stream.Free;
  end;
end;

procedure TMainForm.SaveImage(const AFileName: String; ASize: Double = -1);
var
  img: TCustomBitmap;
  w, h: Integer;
  lineWidth: Integer;
  nailPos: TDoublePointArray = nil;
begin
  img := TPortableNetworkGraphic.Create;
  try
    if ASize <= 0 then
    begin
      w := FOrigImg.Width * 2;
      h := FOrigImg.Height * 2;
      lineWidth := seLineWidth.Value;
    end else
    begin
      w := mmToPx(ASize, ScreenInfo.PixelsPerInchX);
      h := mmToPx(ASize, ScreenInfo.PixelsPerInchY);
      lineWidth := Max(1, Round(seRealLineWidth.Value / 25.4 * 96));
    end;
    img.SetSize(w, h);
    img.Canvas.Brush.Color := clWhite;
    img.Canvas.FillRect(0, 0, img.Width, img.Height);
    InitNails(seNumNails.Value, w, nailPos);
    DrawStringImg(img.Canvas, nailPos, Point(0, 0), lineWidth);
    img.SaveToFile(AFileName);
  finally
    img.Free;
  end;
end;

procedure TMainForm.seImgDiameterChange(Sender: TObject);
begin
  UpdateWireLength;
  UpdateNailDistance;
  UpdateConnectionList;
end;

procedure TMainForm.seNumNailsChange(Sender: TObject);
begin
  Reset;
  UpdateMaxLineCount;
  UpdateNailDistance;
  UpdateConnectionList;
  Paintbox.Invalidate;
end;

procedure TMainForm.tbEditConnectionsChange(Sender: TObject);
begin
  FEditing := tbEditConnections.Checked;
  EditImage.Visible := FEditing;
  if FEditing then
  begin
    DrawEditImage(FConnectionGrid.Row);
    FConnectionGrid.SetFocus;
  end;
  lblEditConnectionsHelp.Visible := FEditing;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  if rbMonochrome.Checked then
  begin
    MakeMonochrome(FWorkImg);
    Paintbox.Invalidate;
  end;
end;

procedure TMainForm.UpdateBtnStates;
begin
  btnSaveImage.Enabled := FUsedNails.Count > 1;
  btnSaveHardwareImage.Enabled := btnSaveImage.Enabled;
  btnSaveConnections.Enabled := btnSaveImage.Enabled;
end;

procedure TMainForm.UpdateCaption;
begin
  if cbFileNames.Text <> '' then
    Caption := MAIN_CAPTION + ' - ' + ExtractFileName(cbFilenames.Text)
  else
    Caption := MAIN_CAPTION
end;

procedure TMainForm.UpdateConnectionList;
const
  FMT = '0.00';
var
  i, r: Integer;
  lineWidth: Integer;
  diam: Double;
  gray: Double;
  factor: Double;
  P, Pprev: TDoublePoint;
  img: TLazIntfImage;
begin
  if (FWorkImg = nil) or (FUsedNails.Count <= 1) then
    FConnectionGrid.RowCount := 2
  else
  begin
    lineWidth := seLineWidth.Value;
    diam := seImgDiameter.Value;
    factor := diam / FWorkImg.Width;
    FConnectionGrid.BeginUpdate;
    img := TLazIntfImage.CreateCompatible(FOrigImg, FOrigImg.Width, FOrigImg.Height);
    try
      MakeWorkImage(img);
      FConnectionGrid.RowCount := FUsedNails.Count + 1;
      Pprev := FNailPos[FUsedNails[0].Index];
      Pprev.X := Pprev.X * factor;
      Pprev.Y := Pprev.Y * factor;
      r := FConnectionGrid.FixedRows;
      for i := 1 to FUsedNails.Count-1 do
      begin
        FConnectionGrid.Cells[0, r] := IntToStr(i);
        FConnectionGrid.Cells[1, r] := IntToStr(FUsedNails[i - 1].Index);
        FConnectionGrid.Cells[2, r] := FormatFloat(FMT, Pprev.X);
        FConnectionGrid.Cells[3, r] := FormatFloat(FMT, Pprev.Y);
        FConnectionGrid.Cells[4, r] := IntToStr(FUsedNails[i].Index);
        P := FNailPos[FUsedNails[i].Index];
        P.X := P.X * factor;
        P.Y := P.Y * factor;
        FConnectionGrid.Cells[5, r] := FormatFloat(FMT, P.X);
        FConnectionGrid.Cells[6, r] := FormatFloat(FMT, P.Y);
        gray := GetAverageLineGray(img, FUsedNails[i-1].Index, FUsedNails[i].Index, lineWidth);
        FConnectionGrid.Cells[7, r] := FormatFloat('0.000', gray);
        Pprev := P;
        inc(r);
      end;
    finally
      img.Free;
      FConnectionGrid.EndUpdate;
    end;
  end;
end;

procedure TMainForm.UpdateMaxLineCount;
var
  n: Integer;
begin
  n := seNumNails.Value;
  if n > 1 then
    Statusbar.Panels[1].Text := Format('Max: %.0n lines', [n * (n-1) / 2])
  else
    Statusbar.Panels[1].Text := '';
end;

procedure TMainForm.UpdateNailDistance;
{
const
  DIST_TEXT = 'Distance between nails: ';
  }
var
  circumference: Double;
  dist: Double;
begin
  if (seNumNails.Value <= 0) then
    infoNailDistance.Caption := ''
  else
  begin
    circumference := seImgDiameter.Value * pi;
    dist := circumference / seNumNails.Value;
    infoNailDistance.Caption := Format('%.2f mm', [dist]);
  end;
end;

procedure TMainForm.UpdateResultsPage;
begin
  UpdateWireLength;
  UpdateNailDistance;
  UpdateConnectionList;
end;

procedure TMainForm.UpdateTotalLineCount;
begin
  if FUsedNails.Count <= 1 then
    Statusbar.Panels[0].Text := ''
  else
    StatusBar.Panels[0].Text := Format('Total: %.0n lines', [1.0*(FUsedNails.Count-1)]);
end;

procedure TMainForm.UpdateWirelength;

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
    infoWireLength.Caption := '';
    exit;
  end;

  R := seImgDiameter.Value * 0.001 / 2;  // "real" image radius, in meters
  len := 0;
  P1 := FNailPos[0];
  for i := 1 to FUsedNails.Count-1 do
  begin
    P2 := FNailPos[FUsedNails[i].Index];
    if FUsedNails[i].Operation = opLineTo then
      len := len + Distance(P1, P2);
    P1 := P2;
  end;
  len := len / (FWorkImg.Width*0.5) * R;
  infoWireLength.Caption := Format('%.2n', [len]);
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  fs: TFormatSettings;
  i: Integer;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini := CreateIni;
  try
    ini.EraseSection('MainForm');
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);

    ini.EraseSection('Parameters');
    ini.WriteInteger('Parameters', 'NumNails', seNumNails.Value);
    ini.WriteBool('Parameters', 'ConvertToGrayScale', rbGrayscale.Checked);
    ini.WriteBool('Parameters', 'ConvertToMonochrome', rbMonochrome.Checked);
    ini.WriteInteger('Parameters', 'MonochromeThreshold', Trackbar.Position);
    ini.WriteBool('Parameters', 'Display StringImage', cgDisplay.Checked[SHOW_STRING_IMAGE]);
    ini.WriteBool('Parameters', 'Display Nails', cgDisplay.Checked[SHOW_NAILS]);
    ini.WriteBool('Parameters', 'Display OrigImage', cgDisplay.Checked[SHOW_ORIG_IMAGE]);
    ini.WriteBool('Parameters', 'Display WorkImage', cgDisplay.Checked[SHOW_WORK_IMAGE]);
    ini.WriteString('Parameters', 'BrightnessTransferExponent', FormatFloat('0.00', fseBrightnessTransferExponent.Value, fs));
    ini.WriteInteger('Parameters', 'NumLines', seNumLines.Value);
    ini.WriteInteger('Parameters', 'LineWidth', seLineWidth.Value);
    ini.WriteString('Parameters', 'RealLineWidth', FormatFloat('0.00', seRealLineWidth.Value, fs));
    ini.WriteString('Parameters', 'ImageDiameter', FormatFloat('0.00', seImgDiameter.Value, fs));

    ini.EraseSection('RecentlyUsed');
    for i := 0 to cbFileNames.Items.Count-1 do
      ini.WriteString('RecentlyUsed', 'File' + IntToStr(i+1), cbFileNames.Items[i]);
  finally
    ini.Free;
  end;
end;

end.

