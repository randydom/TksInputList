unit ksColorButton;

interface

uses Classes, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Types, System.UITypes;

type
  TksColorButtonStyle = (ksbsGray, ksbsGreen, ksbsRed, ksbsBlue, ksbsWhite);

  [ComponentPlatformsAttribute(
    pidAllPlatforms
    )]
  TksColorButton = class(TRectangle)
  private
    FColor: TAlphaColor;
    FFontColor: TAlphaColor;
    FHighlightColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FLabel: TLabel;
    FStyle: TksColorButtonStyle;
    FBorderRadius: single;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetStyle(const Value: TksColorButtonStyle);
    procedure UpdateButton;
    procedure SetColours(ANormal, AHighlight, ABorder, AFont: TAlphaColor);
    procedure SetBorderRadius(const Value: single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Style: TksColorButtonStyle read FStyle write SetStyle default ksbsGreen;
    property Text: string read GetText write SetText;
    property BorderRadius: single read FBorderRadius write SetBorderRadius;
  end;

  procedure Register;

implementation

uses System.UIConsts;

procedure Register;
begin
	RegisterComponents('Graham Murt', [TksColorButton]);
end;

{ TksColorButton }

constructor TksColorButton.Create(AOwner: TComponent);
begin
  inherited;
  FLabel := TLabel.Create(Self);
  FLabel.Stored := False;
  FLabel.Align := TAlignLayout.Client;
  FLabel.StyledSettings := [TStyledSetting.Family,TStyledSetting.Style];
  FLabel.Font.Size := 14;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.TextSettings.FontColor := claWhite;
  FLabel.Text := 'Button';
  AddObject(FLabel);
  Stroke.Color := claBlack;
  FStyle := ksbsGreen;
  Width := 100;
  Height := 40;
  FBorderRadius := 0;
  UpdateButton;
end;

destructor TksColorButton.Destroy;
begin
  FLabel.Free;
  inherited;
end;

procedure TksColorButton.DoMouseLeave;
begin
  inherited;
  Fill.Color := FColor;
end;

function TksColorButton.GetText: string;
begin
  Result := FLabel.Text;
end;

procedure TksColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  Fill.Color := FHighlightColor;
end;

procedure TksColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  Fill.Color := FColor;
end;

procedure TksColorButton.SetStyle(const Value: TksColorButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    UpdateButton;
  end;
end;

procedure TksColorButton.SetBorderRadius(const Value: single);
begin
  if FBorderRadius <> Value then
  begin
    FBorderRadius := Value;
    UpdateButton;
  end;
end;

procedure TksColorButton.SetColours(ANormal, AHighlight, ABorder, AFont: TAlphaColor);
begin
  FColor := ANormal;
  FHighlightColor := AHighlight;
  FBorderColor := ABorder;
  FFontColor := AFont;
  Fill.Color := FColor;
  Stroke.Color := ABorder;
  FLabel.TextSettings.FontColor := AFont;
end;

procedure TksColorButton.SetText(const Value: string);
begin
  FLabel.Text := Value;
end;

procedure TksColorButton.UpdateButton;
begin
  case FStyle of
    ksbsGray: SetColours(claGainsboro, claSilver, claGray, claBlack);
    ksbsGreen: SetColours($FF4BD44B, $FF2FB92F, claForestgreen, clawhite);
    ksbsRed: SetColours(claRed, $FFC90101, $FFC90101, clawhite);
    ksbsWhite: SetColours(claWhite, claGainsboro, claSilver, claDimgray);
    ksbsBlue: SetColours(claSteelblue, $FF80ABCF, claNavy, clawhite);
  end;


end;

end.
