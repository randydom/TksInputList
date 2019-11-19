{*******************************************************************************
*                                                                              *
*  TksInputList                                                                *
*                                                                              *
*  https://github.com/gmurt/TksInputList                                    *
*                                                                              *
*  Copyright 2019 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License forthe specific language governing permissions and          *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksInputList;

interface

uses System.Classes, FMX.Controls, FMX.InertialMovement, System.Types,
  System.Generics.Collections, FMX.Graphics, System.UITypes, FMX.Layouts, FMX.Types,
  FMX.Objects, FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation, System.Threading,
  FMX.ListBox, Json, FMX.Pickers;

  {.$DEFINE DEBUG_BOXES}

const
  C_CORNER_RADIUS = 12;

  {$IFDEF MSWINDOWS}
  C_SCREEN_SCALE = 1;
  C_RIGHT_MARGIN = 20;
  {$ELSE}
  C_SCREEN_SCALE = 2;
  C_RIGHT_MARGIN = 8;
  {$ENDIF}


type
  TksInputList = class;
  TksBaseInputListItem = class;
  TksInputListSelectorItem = class;
  TksInputListEditItem = class;
  TksInputListSwitchItem = class;
  TksInputListCheckBoxItem = class;
  TksInputListButtonItem = class;
  TksInputListTrackBarItem = class;

  TksInputAccessoryType = (atNone, atMore, atCheckmark, atDetail);

  TksInputListItemClickEvent = procedure(Sender: TObject; AItem: TksBaseInputListItem; AID: string) of object;
  TksInputListSelectorItemChangeEvent = procedure(Sender: TObject; AItem: TksInputListSelectorItem; AID, AValue: string) of object;
  TksInputListEditTextChangeEvent = procedure(Sender: TObject; AItem: TksInputListEditItem; AID, AText: string) of object;
  TksInputListSwitchChangeEvent = procedure(Sender: TObject; AItem: TksInputListSwitchItem; AID: string; AIsChecked: Boolean) of object;
  TksInputListCheckBoxChangeEvent = procedure(Sender: TObject; AItem: TksInputListCheckBoxItem; AID: string; AIsChecked: Boolean) of object;
  TksInputListButtonClickEvent = procedure(Sender: TObject; AItem: TksInputListButtonItem; AID: string) of object;
  TksInputListTrackBarItemEvent = procedure(Sender: TObject; AItem: TksInputListTrackBarItem; AID: string; AValue: single) of object;

  TksBaseInputListItem = class
  private
    FksInputList: TksInputList;
    FItemID: string;
    FItemRect: TRectF;
    FImageRect: TRectF;
    FContentRect: TRectF;
    FAccessoryRect: TRectF;
    FAccessory: TksInputAccessoryType;
    FBackground: TAlphaColor;
    FHeight: single;
    FIndex: integer;
    FTitle: string;
    FDetail: string;
    FImage: TBitmap;
    FMouseDown: Boolean;
    FOnChange: TNotifyEvent;
    FSelected: Boolean;
    FShowSelection: Boolean;
    function GetItemRect: TRectF;
    function GetAccessoryWidth(const AAddPadding: Boolean = False): single;
    procedure SetTitle(const Value: string);
    procedure SetHeight(const Value: Single);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetSelected(const Value: Boolean);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetAccessory(const Value: TksInputAccessoryType);
    procedure SetDetail(const Value: string);

  protected
    class function GetClassID: string; virtual; abstract;
    function GetValue: string; virtual;
    procedure SetValue(const AValue: string); virtual;
    procedure SaveStructure(AJson: TJsonObject); virtual;
    procedure LoadStructure(AJson: TJSONObject); virtual;
    procedure UpdateRects; virtual;
    procedure MouseDown; virtual;
    procedure MouseUp(ATapEvent: Boolean); virtual;
    procedure Changed; virtual;
    procedure Reset; virtual;
    property Accessory: TksInputAccessoryType read FAccessory write SetAccessory;
    property BackgroundColor: TAlphaColor read FBackground write SetBackgroundColor;
    property ItemRect: TRectF read GetItemRect;
    property Selected: Boolean read FSelected write SetSelected default False;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default False;
    property Value: string read GetValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AInputList: TksInputList); virtual;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas); virtual;
    procedure DrawSeparators(ACanvas: TCanvas; ATop, ABottom: Boolean);
    procedure SaveToJson(AJson: TJsonObject; AStructure, AData: Boolean);
    procedure LoadFromJson(AJson: TJsonObject; AStructure, AData: Boolean);
    property Height: Single read FHeight write SetHeight;
    property Title: string read FTitle write SetTitle;
    property Detail: string read FDetail write SetDetail;
    property ClassID: string read GetClassID;
    property ID: string read FItemID write FItemID;
  end;

  TksInputListSeperator = class(TksBaseInputListItem)
  protected
    class function GetClassID: string; override;
    function GetValue: string; override;
  public
    procedure DrawToCanvas(ACanvas: TCanvas); override;
  end;

  TksInputListItem = class(TksBaseInputListItem)
  protected
    class function GetClassID: string; override;
  public
    property Accessory;
    property BackgroundColor;
    property ItemRect;
    property Selected;
    property ShowSelection;
  end;


  TksInputListItemWithControl = class(TksInputListItem)
  private
    FControl: TPresentedControl;
    FCached: TBitmap;
    FControlRect: TRectF;
  protected
    procedure UpdateRects; override;
    function CreateControl: TPresentedControl; virtual; abstract;
    procedure PaintControl(ACanvas: TCanvas); virtual;
    procedure UpdateControlPosition;
    procedure ClickControl; virtual;
    procedure Changed; override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    procedure ClearCache;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
  end;

  // inherited to allow access to protected methods.
  TksEdit = class(TEdit);

  TksInputListEditItem = class(TksInputListItemWithControl)
  private
    function GetEdit: TksEdit;
    procedure TextChange(Sender: TObject);
  protected
    class function GetClassID: string; override;
    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    function CreateControl: TPresentedControl; override;
    procedure Reset; override;
    procedure ClickControl; override;
  public
    property Edit: TksEdit read GetEdit;

  end;

  TksInputListSwitchItem = class(TksInputListItemWithControl)
  private
    function GetSwitch: TSwitch;
    procedure SwitchChange(Sender: TObject);
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    function CreateControl: TPresentedControl; override;
    class function GetClassID: string; override;
    procedure Reset; override;

  public
    property Switch: TSwitch read GetSwitch;
  end;

  TksInputListCheckBoxItem = class(TksInputListItemWithControl)
  private
    function GetCheckBox: TCheckBox;
    procedure CheckBoxChange(Sender: TObject);
  protected
    function CreateControl: TPresentedControl; override;
    class function GetClassID: string; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    procedure ClickControl; override;
    procedure Reset; override;
  public
    property CheckBox: TCheckBox read GetCheckBox;
  end;

  TksInputListButtonItem = class(TksInputListItemWithControl)
  private
    function GetButton: TButton;
    procedure DoButtonClick(Sender: TObject);
  protected
    function CreateControl: TPresentedControl; override;
    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    class function GetClassID: string; override;
    procedure ClickControl; override;
  public
    property Button: TButton read GetButton;
  end;

  // inherited to allow access to protected methods.
  TksTrackBar = class(TTrackBar);

  TksInputListTrackBarItem = class(TksInputListItemWithControl)
  private
    function GetTrackBar: TksTrackBar;
    procedure TrackBarChange(Sender: TObject);
  protected
    function CreateControl: TPresentedControl; override;
    class function GetClassID: string; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;

    property TrackBar: TksTrackBar read GetTrackBar;
  end;

  TksInputListSelectorItem = class(TksBaseInputListItem)
  private
    FItems: TStrings;
    FCombo: TComboBox;
    FValue: string;
    procedure DoSelectorChanged(Sender: TObject);
    procedure SetItems(const Value: TStrings);
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    procedure MouseUp(ATapEvent: Boolean); override;
    class function GetClassID: string; override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    property Items: TStrings read FItems write SetItems;
    property Value: string read GetValue write SetValue;
  end;

  TksInputListItems = class(TObjectList<TksBaseInputListItem>)
  private
    FksInputList: TksInputList;
    function GetItemByID(AID: string): TksBaseInputListItem;
  public
    constructor Create(AForm: TksInputList); virtual;
    procedure ItemChange(Sender: TObject);
    procedure DrawToCanvas(ACanvas: TCanvas; AViewPort: TRectF);
    procedure AddSeperator(ATitle: string);
    function AddItem(AID: string; AImg: TBitmap; ATitle: string): TksInputListItem;
    function AddEditBoxItem(AID: string; AImg: TBitmap;
                            ATitle: string;
                            AValue: string;
                            APlaceholder: string;
                            const AKeyboard: TVirtualKeyboardType = TVirtualKeyboardType.Default): TksInputListEditItem;
    procedure AddSwitchItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean);
    procedure AddCheckBoxItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean);
    procedure AddButtonItem(AID: string; AImg: TBitmap; ATitle, AButtonTitle: string);
    procedure AddTrackBar(AID: string; AImg: TBitmap; ATitle: string; APos, AMax: integer);
    procedure AddItemSelector(AID: string; AImg: TBitmap; ATitle, ASelected: string; AItems: array of string); overload;
    procedure AddItemSelector(AID: string; AImg: TBitmap; ATitle, ASelected: string; AItems: TStrings); overload;
    property ItemByID[AID: string]: TksBaseInputListItem read GetItemByID;
  end;

  TksInputListCanvas = class(TPaintBox)
  protected
    procedure Paint; override;
  end;

  [ComponentPlatformsAttribute(
     pidWin32 or
     pidWin64 or
     pidiOSSimulator32 or pidiOSSimulator64 or
     pidiOSDevice32 or pidiOSDevice64
    )]
  TksInputList = class(TVertScrollBox)
  private
    FPickerService: IFMXPickerService;
    FCanvas: TksInputListCanvas;
    FItems: TksInputListItems;
    FLastScrollPos: single;
    FScrollMonitor: ITask;
    FControlsVisible: Boolean;
    FUpdateCount: integer;
    FLastScrollChange: TDateTime;
    FMouseDownPos: TPointF;
    FMousePos: TPointF;
    FMouseDownItem: TksBaseInputListItem;
    FOnSelectorItemSelected: TksInputListSelectorItemChangeEvent;
    FOnEditItemTextChange: TksInputListEditTextChangeEvent;
    FOnItemClick: TksInputListItemClickEvent;
    FOnSwitchChange: TksInputListSwitchChangeEvent;
    FOnCheckBoxChange: TksInputListCheckBoxChangeEvent;
    FOnItemButtonClick: TksInputListButtonClickEvent;
    FOnItemTrackBarChange: TksInputListTrackBarItemEvent;
    procedure UpdateItemRects;
    procedure RedrawItems;
    procedure CreateScrollMonitor;
    procedure ShowOnScreenControls;
    function GetIsScrolling: Boolean;
    procedure HidePickers;
  protected
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
                                     const ContentSizeChanged: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HideAllControls;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure ClearItems;
    procedure Reset;

    procedure LoadFromJson(AJsonData: string; AStructure: Boolean; AData: Boolean); overload;
    procedure LoadFromJson(AJson: TJsonObject; AStructure: Boolean; AData: Boolean); overload;
    procedure SaveToJson(AJson: TJsonObject; AStructure: Boolean; AData: Boolean); overload;

    property IsScrolling: Boolean read GetIsScrolling;
    property Items: TksInputListItems read FItems;

  published
    property VScrollBar;

    property OnItemClick: TksInputListItemClickEvent read FOnItemClick write FOnItemClick;
    property OnSelectorItemSelected: TksInputListSelectorItemChangeEvent read FOnSelectorItemSelected write FOnSelectorItemSelected;
    property OnEditItemTextChange: TksInputListEditTextChangeEvent read FOnEditItemTextChange write FOnEditItemTextChange;
    property OnItemSwitchChanged: TksInputListSwitchChangeEvent read FOnSwitchChange write FOnSwitchChange;
    property OnItemCheckBoxChanged: TksInputListCheckBoxChangeEvent read FOnCheckBoxChange write FOnCheckBoxChange;
    property OnItemButtonClick: TksInputListButtonClickEvent read FOnItemButtonClick write FOnItemButtonClick;
    property OnItemTrackBarChange: TksInputListTrackBarItemEvent read FOnItemTrackBarChange write FOnItemTrackBarChange;
  end;

  procedure Register;

implementation

uses System.UIConsts, SysUtils, FMX.Forms, FMX.DialogService, DateUtils,
  Math, FMX.Styles, FMX.Styles.Objects, System.NetEncoding, FMX.Platform;

procedure Register;
begin
	RegisterComponents('Graham Murt', [TksInputList]);
end;


type
  TInputListAccessoryImage = class
  private
    FAccessoryType: TksInputAccessoryType;
    FBitmap: TBitmap;
  private
    constructor Create(AType: TksInputAccessoryType; ABmp: TBitmap); virtual;
    destructor Destroy; override;
    property AccessoryType: TksInputAccessoryType read FAccessoryType;
    property Bitmap: TBitmap read FBitmap;
  end;

  TInputListAccessoryImages = class(TObjectList<TInputListAccessoryImage>)
  private
    function GetItemByAccessory(AType: TksInputAccessoryType): TInputListAccessoryImage;
  public
    constructor Create; virtual;
    property ItemByAccessory[AType: TksInputAccessoryType]: TInputListAccessoryImage read GetItemByAccessory;
  end;

var
  TempForm: TForm;
  AAccessoriesList: TInputListAccessoryImages;

function AccessoryToStr(AAcc: TksInputAccessoryType): string;
begin
  Result := '';
  case AAcc of
    atMore: Result := 'more';
    atCheckmark: Result := 'checkmark';
    atDetail: Result := 'detail';
  end;
end;

function StrToAccessory(AAcc: string): TksInputAccessoryType;
begin
  AAcc := Trim(AAcc.ToLower);
  Result := atNone;
  if AAcc = 'more' then Result := atMore;
  if AAcc = 'checkmark' then Result := atCheckmark;
  if AAcc = 'detail' then Result := atDetail;
end;

function CreateListItem(AInputList: TksInputList; AClassID: string): TksBaseInputListItem;
begin
  Result := nil;
  if AClassID = TksInputListSeperator.GetClassID then Result := TksInputListSeperator.Create(AInputList);
  if AClassID = TksInputListItem.GetClassID then Result := TksInputListItem.Create(AInputList);
  if AClassID = TksInputListEditItem.GetClassID then Result := TksInputListEditItem.Create(AInputList);
  if AClassID = TksInputListSwitchItem.GetClassID then Result := TksInputListSwitchItem.Create(AInputList);
  if AClassID = TksInputListCheckBoxItem.GetClassID then Result := TksInputListCheckBoxItem.Create(AInputList);
  if AClassID = TksInputListButtonItem.GetClassID then Result := TksInputListButtonItem.Create(AInputList);
  if AClassID = TksInputListTrackBarItem.GetClassID then Result := TksInputListTrackBarItem.Create(AInputList);
  if AClassID = TksInputListSelectorItem.GetClassID then Result := TksInputListSelectorItem.Create(AInputList);
end;

function BmpToBase64(AImg: TBitmap): string;
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
begin
  AStream := TMemoryStream.Create;
  AEncoded := TStringStream.Create;
  try
    if AImg <> nil then
      AImg.SaveToStream(AStream);
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, AEncoded);
    Result := AEncoded.DataString;
  finally
    AStream.Free;
    AEncoded.Free;
  end;
end;

procedure Base64ToBmp(AData: string; AImg: TBitmap);
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
begin
  AEncoded := TStringStream.Create(AData);
  AStream := TMemoryStream.Create;
  try
    AEncoded.Position := 0;
    TNetEncoding.Base64.Decode(AEncoded, AStream);
    AStream.Position := 0;
    AImg.LoadFromStream(AStream);
  finally
    AStream.Free;
    AEncoded.Free;
  end;
end;

function GetAccessoryFromResource(AStyleName: array of string; const AState: string = ''): TBitmap;
var
  AStyleObj: TStyleObject;
  AImgRect: TBounds;
  r: TRectF;
  ABitmapLink: TBitmapLinks;
  AImageMap: TBitmap;
  I: integer;
  AScale: single;
  ICount: integer;
  AMap: TBitmap;
begin
  AMap := TBitmap.Create;
  Result := TBitmap.Create;
  AScale := C_SCREEN_SCALE;
  AStyleObj := TStyleObject(TStyleManager.ActiveStyle(nil));

  for ICount := Low(AStyleName) to High(AStyleName) do
    AStyleObj := TStyleObject(AStyleObj.FindStyleResource(AStyleName[ICount]));

  if AStyleObj <> nil then
  begin
    if AMap.IsEmpty then
    begin
      for i := 0 to (AStyleObj as TStyleObject).Source.MultiResBitmap.Count-1 do
      begin
        AScale := (AStyleObj as TStyleObject).Source.MultiResBitmap[i].Scale;
        if Round(AScale) <= AScale then
        begin
          AScale := Round(AScale);
          Break;
        end;
      end;
      AImageMap := ((AStyleObj as TStyleObject).Source.MultiResBitmap.Bitmaps[AScale]);
      AMap.SetSize(Round(AImageMap.Width), Round(AImageMap.Height));
      AMap.Clear(claNull);

      AMap.Canvas.BeginScene;
      try
        AMap.Canvas.DrawBitmap(AImageMap,
                                    RectF(0, 0, AImageMap.Width, AImageMap.Height),
                                    RectF(0, 0, AMap.Width, AMap.Height),
                                    1,
                                    True);
      finally
        AMap.Canvas.EndScene;
      end;
    end;

    ABitmapLink := nil;
    if AStyleObj = nil then
      Exit;
    if (AStyleObj.ClassType = TCheckStyleObject) then
    begin
      if AState = 'checked' then
        ABitmapLink := TCheckStyleObject(AStyleObj).ActiveLink
      else
        ABitmapLink := TCheckStyleObject(AStyleObj).SourceLink
    end;

    if ABitmapLink = nil then
      ABitmapLink := AStyleObj.SourceLink;

    AImgRect := ABitmapLink.LinkByScale(AScale, True).SourceRect;


    Result.SetSize(Round(AImgRect.Width), Round(AImgRect.Height));
    Result.Clear(claNull);
    Result.Canvas.BeginScene;

    r := AImgRect.Rect;

    Result.Canvas.DrawBitmap(AMap,
                             r,
                             RectF(0, 0, Result.Width, Result.Height),
                             1,
                             True);
    Result.Canvas.EndScene;
  end;
  AMap.Free;
end;


{ TksInputList }

procedure TksInputList.BeginUpdate;
begin
  //inherited;
  Inc(FUpdateCount);
end;

procedure TksInputList.ClearItems;
begin
  BeginUpdate;
  try
    FItems.Clear;
  finally
    EndUpdate;
  end;
end;

constructor TksInputList.Create(AOwner: TComponent);
begin
  inherited;
  if TempForm = nil then

    TempForm := TForm.CreateNew(nil);

  FPickerService := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService);

  FControlsVisible := False;
  FLastScrollPos := 0;
  FLastScrollChange := Now;
  FItems := TksInputListItems.Create(Self);
  FCanvas := TksInputListCanvas.Create(Self);
  FCanvas.Align := TAlignLayout.Top;

  FCanvas.HitTest := False;
  FCanvas.Stored := False;
  AddObject(FCanvas);
  UpdateItemRects;

  CreateScrollMonitor;


end;

procedure TksInputList.CreateScrollMonitor;
begin
  FScrollMonitor := TTask.Create (procedure ()
  begin
    while not Application.Terminated do
    begin
      sleep (100);
      if (FItems.Count > 0) and (MilliSecondsBetween(FLastScrollPos, Now) > 300) then
      begin
        if (FLastScrollPos = VScrollBarValue) and (FControlsVisible = False) then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowOnScreenControls;
            end);
        end

      end;
      FLastScrollPos := VScrollBarValue;

    end;
  end);
  FScrollMonitor.Start;
end;

destructor TksInputList.Destroy;
begin
  FItems.Free;
  FCanvas.Free;
  inherited;
end;


procedure TksInputList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateItemRects;
end;

function TksInputList.GetIsScrolling: Boolean;
begin
  Result := ((FMousePos.Y) < (FMouseDownPos.Y - 10)) or ((FMousePos.Y) > (FMouseDownPos.Y + 10));
end;

procedure TksInputList.HideAllControls;
var
  AItem: TksBaseInputListItem;
begin
  inherited;
  if (FUpdateCount > 0) or (FControlsVisible = False) then
    Exit;
  BeginUpdate;
  try
    for AItem in FItems do
    begin
      if AItem is TksInputListItemWithControl then
      begin
        TempForm.AddObject((AItem as TksInputListItemWithControl).FControl);
        (AItem as TksInputListItemWithControl).FControl.ApplyStyleLookup;
      end;
    end;
    FControlsVisible := False;
  finally
    EndUpdate;
  end;
end;

procedure TksInputList.HidePickers;
begin
  if FPickerService <> nil then
    FPickerService.CloseAllPickers;
end;

procedure TksInputList.LoadFromJson(AJsonData: string; AStructure,
  AData: Boolean);
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.ParseJSONValue(AJsonData) as TJSONObject;
  try
    LoadFromJson(AJson, AStructure, AData);
  finally
    AJson.Free;
  end;
end;

procedure TksInputList.LoadFromJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
var
  AObj: TJSONObject;
  AArray: TJSONArray;
  ICount: integer;
  AItem: TksBaseInputListItem;
begin
  BeginUpdate;
  try
    if AStructure then
      ClearItems;
    AArray := (AJson.Values['items'] as TJSONArray);
    begin
      for ICount := 0 to AArray.Count-1 do
      begin
        AObj := AArray.Items[ICount] as TJSONObject;
        if AStructure then
        begin
          AItem := CreateListItem(Self, AObj.Values['class_id'].Value);
          FItems.Add(AItem);
        end
        else
          AItem := FItems.ItemByID[AObj.Values['id'].Value];
        if AItem <> nil then
          AItem.LoadFromJson(AObj, AStructure, AData);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TksInputList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  AItem: TksBaseInputListItem;
begin
  inherited;
  HidePickers;
  y := y + VScrollBarValue;
  Root.SetFocused(nil);

  FMouseDownPos := PointF(X, Y);
  FMousePos := FMouseDownPos;
  for AItem in FItems do
  begin

    if PtInRect(AItem.ItemRect, PointF(x, y)) then
    begin
      FMouseDownItem := AItem;
      AItem.MouseDown;
    end;
  end;
end;

procedure TksInputList.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  y := y + VScrollBarValue;
  FMousePos := PointF(X, Y);
end;

procedure TksInputList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  ATapEvent: Boolean;
begin
  inherited;
  y := y + VScrollBarValue;
  ATapEvent := ((Y) > (FMouseDownPos.Y - 10)) and ((Y) < (FMouseDownPos.Y + 10));
  if FMouseDownItem <> nil then
  begin
    FMouseDownItem.MouseUp(ATapEvent);
    FMouseDownItem := nil;
  end;
end;

procedure TksInputList.UpdateItemRects;
var
  ICount: integer;
  AItem: TksBaseInputListItem;
  ATop: single;
  AHeight: single;
begin
  ATop := 0;
  AHeight := 0;
  for ICount := 0 to FItems.Count-1 do
  begin
    AItem := FItems[ICount];
    AItem.FItemRect := RectF(0, ATop, Width, ATop + AItem.Height);
    AItem.FIndex := ICount;
    ATop := AItem.FItemRect.Bottom;
    AHeight := AHeight + AItem.FItemRect.Height;
  end;
  FCanvas.Height := AHeight;
end;

procedure TksInputList.ViewportPositionChange(const OldViewportPosition,
  NewViewportPosition: TPointF; const ContentSizeChanged: boolean);
begin
  inherited;
  FLastScrollChange := Now;
  HideAllControls;
end;

procedure TksInputList.RedrawItems;
var
  r: TRectF;
begin
  BeginUpdate;
  try
    r := ClipRect;
    OffsetRect(r, 0, VScrollBarValue);
    FItems.DrawToCanvas(FCanvas.Canvas, r);
  finally
    EndUpdate;
  end;
end;

procedure TksInputList.Reset;
var
  AItem: TksBaseInputListItem;
begin
  BeginUpdate;
  try
    for AItem in FItems do
    begin
      AItem.Reset;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TksInputList.Resize;
begin
  inherited;
  UpdateItemRects;
  ShowOnScreenControls;
end;

procedure TksInputList.SaveToJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
var
  AArray: TJSONArray;
  AObj: TJSONObject;
  AItem: TksBaseInputListItem;
begin
  AArray := TJSONArray.Create;
  AJson.AddPair('items', AArray);
  for AItem in FItems do
  begin
    AObj := TJSONObject.Create;
    AItem.SaveToJson(AObj, AStructure, AData);
    AArray.Add(AObj);
  end;
end;

procedure TksInputList.ShowOnScreenControls;
var
  AItem: TksBaseInputListItem;
  ACtrlItem: TksInputListItemWithControl;
  r: TRectF;
begin

  if FUpdateCount > 0 then
    Exit;
  r := ClipRect;
  OffsetRect(r, 0, VScrollBarValue);

  for AItem in FItems do
  begin
    if (AItem is TksInputListItemWithControl) then
    begin
      ACtrlItem := (AItem as TksInputListItemWithControl);
      if IntersectRect(r, ACtrlItem.FItemRect) then
      begin
        if ACtrlItem.FControl.Parent = TempForm then
        begin
          ACtrlItem.UpdateControlPosition;
          ACtrlItem.FControl.Visible := True;
          ACtrlItem.ClearCache;
          Self.AddObject(ACtrlItem.FControl);
        end;

      end;
    end;
  end;
  FControlsVisible := True;

end;

{ TksBaseInputListItem }

procedure TksBaseInputListItem.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


constructor TksBaseInputListItem.Create(AInputList: TksInputList);
begin
  inherited Create;
  FksInputList := AInputList;
  FImage := TBitmap.Create;
  FHeight := 50;
  FItemID := '';
  FBackground := claWhite;
  FSelected := False;
  FShowSelection := False;
end;

destructor TksBaseInputListItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TksBaseInputListItem.DrawSeparators(ACanvas: TCanvas; ATop, ABottom: Boolean);
begin
  ACanvas.Stroke.Color := claBlack;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := 0.5;
  if ATop then ACanvas.DrawRectSides(FItemRect, 0, 0, AllCorners, 1, [TSide.Top]);
  if ABottom then
  begin
    ACanvas.DrawRectSides(FItemRect, 0, 0, AllCorners, 1, [TSide.Bottom]);
  end;
end;

procedure TksBaseInputListItem.DrawToCanvas(ACanvas: TCanvas);
var
  AState: TCanvasSaveState;
  AAccRect: TRectF;
  AAcc: TBitmap;
begin
  UpdateRects;
  AState := ACanvas.SaveState;
  try
    ACanvas.IntersectClipRect(FItemRect);
    ACanvas.Fill.Color := FBackground;

    if (FSelected) and (FShowSelection) then
      ACanvas.Fill.Color := claGainsboro;

    ACanvas.FillRect(FItemRect, 0, 0, AllCorners, 1);

    {$IFDEF DEBUG_BOXES}
    ACanvas.Stroke.Color := claRed;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Thickness := 2;
    ACanvas.DrawRect(FContentRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);
    ACanvas.Stroke.Color := claGreen;

    ACanvas.DrawRect(FContentRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);

    ACanvas.Stroke.Color := claBlue;
    ACanvas.DrawRect(FAccessoryRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);

    ACanvas.Stroke.Color := claPink;
    ACanvas.DrawRect(FImageRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);
    {$ENDIF}


    if FAccessory <> atNone then
    begin
      AAcc := AAccessoriesList.ItemByAccessory[FAccessory].Bitmap;
      if AAcc <> nil then
      begin
        AAccRect := RectF(0, 0, AAcc.Width, AAcc.Height);

        ACanvas.DrawBitmap(AAcc,
                           AAccRect,
                           RectF(0, 0, AAccRect.Width/2, AAccRect.Height/2).PlaceInto(FAccessoryRect),
                           1,
                           True);

        {$IFDEF DEBUG_BOXES}
        ACanvas.Stroke.Color := claBlack;
        ACanvas.Stroke.Kind := TBrushKind.Solid;
        ACanvas.Stroke.Thickness := 1;
        ACanvas.DrawRect(FAccessoryRect, 0, 0, AllCorners, 1);
        {$ENDIF}
      end;
    end;

    if FImage.IsEmpty = False then
    begin
      FImageRect.Inflate(-4, -4);
      ACanvas.DrawBitmap(FImage, FImage.BoundsF, FImage.BoundsF.PlaceInto(FImageRect),1, True);
    end;
    ACanvas.Fill.Color := claBlack;
    ACanvas.FillText(FContentRect, FTitle, False, 1, [], TTextAlign.Leading, TTextAlign.Center);

    ACanvas.FillText(FContentRect, FDetail, False, 1, [], TTextAlign.Trailing, TTextAlign.Center);
  finally
    ACanvas.RestoreState(AState);
  end;
end;

function TksBaseInputListItem.GetAccessoryWidth(const AAddPadding: Boolean = False): single;
begin
  Result := 0;
  if FAccessory <> atNone then
  begin
    Result := AAccessoriesList.ItemByAccessory[FAccessory].Bitmap.Width;
    if AAddPadding then
      Result := Result + 4;
  end;
end;

function TksBaseInputListItem.GetItemRect: TRectF;
begin
  Result := FItemRect;
end;

function TksBaseInputListItem.GetValue: string;
begin
  Result := '';
end;

procedure TksBaseInputListItem.LoadFromJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
begin
  if AStructure then  LoadStructure(AJson);
  if AData then Value := AJson.Values['value'].Value;
  
end;

procedure TksBaseInputListItem.LoadStructure(AJson: TJSONObject);
begin
  if AJson.Values['image'] <> nil then
    Base64ToBmp(AJson.Values['image'].Value, FImage);
  FItemID := AJson.Values['id'].Value;
  FAccessory := StrToAccessory(AJson.Values['acc'].Value);
  FBackground := StringToAlphaColor(AJson.Values['background'].Value);
  FHeight := StrToFloat(AJson.Values['height'].Value);
  FIndex := StrToInt(AJson.Values['index'].Value);
  FTitle := AJson.Values['title'].Value;
  FDetail := AJson.Values['detail'].Value;

  FShowSelection := StrToBool(AJson.Values['show_selection'].Value);
end;

procedure TksBaseInputListItem.MouseDown;
var
  ATask: ITask;
begin
  FMouseDown := True;
  if FShowSelection then
  begin
    ATask := TTask.Create (procedure ()
    begin
      Sleep(50);
      if FksInputList.IsScrolling then
        Exit;
      if FMouseDown then
      begin
        TThread.Synchronize(nil,procedure
        begin
          Selected := True;
        end);
      end;
    end);
    ATask.Start;
  end;
end;

procedure TksBaseInputListItem.MouseUp(ATapEvent: Boolean);
var
  ATask: ITask;
begin
  if FMouseDown then
  begin

    aTask := TTask.Create (procedure ()
         begin
           Sleep(200);
           FMouseDown := False;
           if (FSelected) then
           begin
             TThread.Synchronize(nil,procedure
                          begin
                          Selected := False;
                          if ATapEvent then
                          begin
                            if Assigned(FksInputList.OnItemClick) then
                              FksInputList.OnItemClick(FksInputList, Self, ID);
                          end

              end);
             end;
         end);
       aTask.Start;
    end;

end;

procedure TksBaseInputListItem.Reset;
begin
  //
end;

procedure TksBaseInputListItem.SaveStructure(AJson: TJsonObject);
begin
  AJson.AddPair('class_id', GetClassID);
  if not FImage.IsEmpty then
    AJson.AddPair('image', BmpToBase64(FImage));
  AJson.AddPair('acc', AccessoryToStr(FAccessory));
  AJson.AddPair('background', AlphaColorToString(FBackground));
  AJson.AddPair('height', FloatToStr(FHeight));
  AJson.AddPair('index', IntToStr(FIndex));
  AJson.AddPair('title', FTitle);
  AJson.AddPair('detail', FDetail);
  AJson.AddPair('show_selection', BoolToStr(FShowSelection, True));
end;

procedure TksBaseInputListItem.SaveToJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
begin
  AJson.AddPair('id', FItemID);
  if AStructure then
    SaveStructure(AJson);
  if AData then
    AJson.AddPair('value', Value);
end;

procedure TksBaseInputListItem.SetAccessory(const Value: TksInputAccessoryType);
begin
  if FAccessory <> Value then
  begin
    FAccessory := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackground := Value;
  Changed;
end;

procedure TksBaseInputListItem.SetDetail(const Value: string);
begin
  if FDetail <> Value then
  begin
    FDetail := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetHeight(const Value: Single);
begin
  FHeight := Value;
  Changed;
end;

procedure TksBaseInputListItem.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
  Changed;
end;

procedure TksBaseInputListItem.SetShowSelection(const Value: Boolean);
begin
  FShowSelection := Value;
end;

procedure TksBaseInputListItem.SetTitle(const Value: string);
begin
  FTitle := Value;
  Changed;
end;

procedure TksBaseInputListItem.SetValue(const AValue: string);
begin
  // overridden in descendant classes.
end;

procedure TksBaseInputListItem.UpdateRects;
var
  AAccessoryWidth: single;
begin
  FAccessoryRect := FAccessoryRect.Empty;
  FImageRect := FImageRect.Empty;

  FContentRect := FItemRect;
  FContentRect.Left := FContentRect.Left + 8;
  FContentRect.Right := FContentRect.Right - C_RIGHT_MARGIN;

  // add image rect...
  if not FImage.IsEmpty then
  begin
    FImageRect := FContentRect;
    FImageRect.Right := FContentRect.Left+32;
    FContentRect.Left := FImageRect.Right;
  end;

  if FAccessory <> atNone then
  begin
    // add accessory rect
    AAccessoryWidth := GetAccessoryWidth(False);
    FAccessoryRect := FContentRect;
    FAccessoryRect.Left := FContentRect.Right-AAccessoryWidth;
    FContentRect.Right := FAccessoryRect.Left;
  end;

end;

{ TksInputListItems }

procedure TksInputListItems.AddButtonItem(AID: string; AImg: TBitmap; ATitle,
  AButtonTitle: string);
var
  AItem: TksInputListButtonItem;
begin
  AItem := TksInputListButtonItem.Create(FksInputList);
  AItem.FItemID := AID;
  AItem.FImage.Assign(AImg);
  AItem.Title := ATitle;
  AItem.Button.Text := AButtonTitle;
  AItem.Button.DisableFocusEffect := True;
  AItem.Button.CanFocus := False;
  AItem.OnChange := ItemChange;
  Add(AItem);
  ItemChange(Self);
end;

procedure TksInputListItems.AddCheckBoxItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean);
var
  AItem: TksInputListCheckBoxItem;
begin
  AItem := TksInputListCheckBoxItem.Create(FksInputList);
  AItem.FImage.Assign(AImg);
  AItem.FItemID := AID;
  AItem.Title := ATitle;
  AItem.CheckBox.IsChecked := AState;
  AItem.CheckBox.OnChange := AItem.CheckBoxChange;
  AItem.OnChange := ItemChange;
  Add(AItem);
  ItemChange(Self);
end;

function TksInputListItems.AddEditBoxItem(AID: string;
                                          AImg: TBitmap;
                                          ATitle: string;
                                          AValue: string;
                                          APlaceholder: string;
                                          const AKeyboard: TVirtualKeyboardType = TVirtualKeyboardType.Default): TksInputListEditItem;
begin
  Result := TksInputListEditItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.Edit.KeyboardType := AKeyboard;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.Edit.TextPrompt := APlaceholder;
  Result.Edit.Text := AValue;
  Result.OnChange := ItemChange;
  Result.Edit.OnChangeTracking := Result.TextChange;
  Result.Edit.ApplyStyleLookup;

  Add(Result);
  ItemChange(Self);
end;

procedure TksInputListItems.AddSeperator(ATitle: string);
var
  AItem: TksInputListSeperator;
begin
  AItem := TksInputListSeperator.Create(FksInputList);
  AItem.FItemID := GuidToString(TGUID.NewGuid);
  AItem.Title := ATitle;
  Add(AItem);
  ItemChange(Self);
end;

procedure TksInputListItems.AddSwitchItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean);
var
  AItem: TksInputListSwitchItem;
begin
  AItem := TksInputListSwitchItem.Create(FksInputList);
  AItem.FItemID := AID;
  AItem.FImage.Assign(AImg);
  AItem.Title := ATitle;
  AItem.Switch.IsChecked := AState;
  AItem.Switch.OnSwitch := AItem.SwitchChange;
  AItem.OnChange := ItemChange;
  Add(AItem);
  ItemChange(Self);
end;

procedure TksInputListItems.AddTrackBar(AID: string; AImg: TBitmap; ATitle: string; APos,
  AMax: integer);
var
  AItem: TksInputListTrackBarItem;
begin
  AItem := TksInputListTrackBarItem.Create(FksInputList);
  AItem.FItemID := AID;
  AItem.FImage.Assign(AImg);
  AItem.Title := ATitle;
  AItem.TrackBar.Max := AMax;
  AItem.TrackBar.Value := AMax;
  AItem.OnChange := ItemChange;
  AItem.TrackBar.OnTracking := AItem.TrackBarChange;

  Add(AItem);
  ItemChange(Self);
end;

function TksInputListItems.AddItem(AID: string; AImg: TBitmap; ATitle: string): TksInputListItem;
begin
  Result := TksInputListItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

procedure TksInputListItems.AddItemSelector(AID: string; AImg: TBitmap; ATitle,
  ASelected: string; AItems: TStrings);
var
  AItem: TksInputListSelectorItem;
begin
  AItem := TksInputListSelectorItem.Create(FksInputList);
  AItem.FItemID := AID;
  AItem.Items.Assign(AItems);
  AItem.FImage.Assign(AImg);
  AItem.Title := ATitle;
  AItem.Accessory := atMore;
  AItem.Value := ASelected;
  AItem.OnChange := ItemChange;
  Add(AItem);
  ItemChange(Self);
end;

procedure TksInputListItems.AddItemSelector(AID: string; AImg: TBitmap; ATitle,
  ASelected: string; AItems: array of string);
var
  AStrings: TStrings;
  s: string;
begin
  AStrings := TStringList.Create;
  try
    for s in AItems do
      AStrings.Add(s);
    AddItemSelector(AId, AImg, ATitle, ASelected, AStrings);
  finally
    AStrings.Free;
  end;
end;

constructor TksInputListItems.Create(AForm: TksInputList);
begin
  inherited Create(True);
  FksInputList := AForm;
end;

procedure TksInputListItems.DrawToCanvas(ACanvas: TCanvas; AViewPort: TRectF);
var
  AItem: TksBaseInputListItem;
  ICount: integer;
begin

  for AItem in Self do
  begin
    if IntersectRect(AViewPort, AItem.FItemRect) then
      AItem.DrawToCanvas(ACanvas);
  end;

  for ICount := 0 to Count-1 do
  begin
    AItem := Items[ICount];
    if IntersectRect(AViewPort, AItem.FItemRect) then
      AItem.DrawSeparators(ACanvas, True, ICount = Count-1);
  end;

end;

function TksInputListItems.GetItemByID(AID: string): TksBaseInputListItem;
var
  AItem: TksBaseInputListItem;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.ID = AID then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

procedure TksInputListItems.ItemChange(Sender: TObject);
begin

  FksInputList.InvalidateRect(FksInputList.ClipRect);
  FksInputList.ShowOnScreenControls;
end;

{ TksInputListCanvas }

procedure TksInputListCanvas.Paint;
begin
  inherited;
  (Owner as TksInputList).RedrawItems;
end;

{ TksInputListEditItem }


procedure TksInputListEditItem.ClickControl;
begin
  inherited;
  TEdit(FControl).SelStart := TEdit(FControl).Text.Length;

end;


function TksInputListEditItem.CreateControl: TPresentedControl;
begin
  Result := TksEdit.Create(nil);
  (Result as TksEdit).Width := 150;
end;

class function TksInputListEditItem.GetClassID: string;
begin
  Result := '{099048A9-EC3B-4931-89DF-61C938B3F571}';
end;

function TksInputListEditItem.GetEdit: TksEdit;
begin
  Result := (FControl as TksEdit);
end;

function TksInputListEditItem.GetValue: string;
begin
  Result := Edit.Text;
end;

procedure TksInputListEditItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;
  Edit.KeyboardType := TVirtualKeyboardType(StrToIntDef(AJson.Values['keyboard'].Value, 0));
end;

procedure TksInputListEditItem.Reset;
begin
  inherited;
  Edit.Text := '';
end;

procedure TksInputListEditItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('keyboard', IntToStr(Ord(Edit.KeyboardType)));

end;

procedure TksInputListEditItem.SetValue(const AValue: string);
begin
  Edit.Text := AValue;
end;

procedure TksInputListEditItem.TextChange(Sender: TObject);
begin
  if Assigned(FksInputList.OnEditItemTextChange) then
    FksInputList.OnEditItemTextChange(FksInputList, Self, ID, Value);
end;

{ TksInputListItemWithControl }

procedure TksInputListItemWithControl.Changed;
begin
  inherited;
  UpdateRects;
  UpdateControlPosition;
end;

procedure TksInputListItemWithControl.ClearCache;
begin
  FCached.SetSize(0,0);
end;

procedure TksInputListItemWithControl.ClickControl;
begin
  FksInputList.HidePickers;
  FControl.SetFocus;
end;

constructor TksInputListItemWithControl.Create(AInputList: TksInputList);
begin
  inherited;
  FControl := CreateControl;
  FControl.Visible := True;
  FControl.Parent := TempForm;
  FControl.ApplyStyleLookup;
  FCached := TBitmap.Create;
end;


destructor TksInputListItemWithControl.Destroy;
begin
  FControl.DisposeOf;
  FCached.Free;
  inherited;
end;

procedure TksInputListItemWithControl.DrawToCanvas(ACanvas: TCanvas);
var
  AScreenScale: single;
begin
  inherited;
  {$IFDEF DEBUG_BOXES}
  ACanvas.Stroke.Color := claBlack;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := 2;
  ACanvas.DrawRect(FControl.BoundsRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);
  {$ENDIF}

  if FControl.Parent <> TempForm then
    Exit;

  AScreenScale := C_SCREEN_SCALE;

  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := 0.5;

  if FCached.IsEmpty then
  begin
    FCached.BitmapScale := AScreenScale;
    FCached.SetSize(Round(FControl.Width*AScreenScale), Round(FControl.Height*AScreenScale));
    FCached.Canvas.BeginScene;
    FCached.Clear(claNull);
    PaintControl(FCached.Canvas);
    FCached.Canvas.EndScene;
  end;
  ACanvas.DrawBitmap(FCached, Rect(0, 0, FCached.Width, FCached.Height), FControl.BoundsRect.PlaceInto(FControlRect, THorzRectAlign.Right), 1);
end;

procedure TksInputListItemWithControl.PaintControl(ACanvas: TCanvas);
begin
  FControl.PaintTo(ACanvas, RectF(0, 0, FControl.Width, FControl.Height));
end;


procedure TksInputListItemWithControl.Reset;
begin
  inherited;
  FCached.SetSize(0, 0);
end;

procedure TksInputListItemWithControl.UpdateControlPosition;
begin
  UpdateRects;
  FControl.BoundsRect := FControl.BoundsRect.PlaceInto(FControlRect, THorzRectAlign.Right);
end;

procedure TksInputListItemWithControl.UpdateRects;
begin
  inherited;
  FControlRect := FContentRect;
  FControlRect.Left := FControlRect.Right - FControl.Width;
  FContentRect.Right := FControlRect.Left;
end;

{ TksInputListSwitchItem }

function TksInputListSwitchItem.CreateControl: TPresentedControl;
begin
  Result := TSwitch.Create(nil);
end;

class function TksInputListSwitchItem.GetClassID: string;
begin
  Result := '{C9533F62-6097-4AB2-B353-C54F83B29EEF}';
end;

function TksInputListSwitchItem.GetSwitch: TSwitch;
begin
  Result := FControl as TSwitch;
end;

function TksInputListSwitchItem.GetValue: string;
begin
  Result := BoolToStr(Switch.IsChecked, True);
end;

procedure TksInputListSwitchItem.Reset;
begin
  inherited;
  Switch.IsChecked := False;
end;

procedure TksInputListSwitchItem.SetValue(const AValue: string);
begin
  inherited;
  Switch.IsChecked := StrToBool(AValue);
end;

procedure TksInputListSwitchItem.SwitchChange(Sender: TObject);
var
  ATask: ITask;
begin
  ATask := TTask.Create (procedure ()
   begin
     TThread.Synchronize(nil,procedure
      begin
        if Assigned(FksInputList.OnItemSwitchChanged) then
          FksInputList.OnItemSwitchChanged(FksInputList, Self, ID, Switch.IsChecked);
      end);
   end);
  ATask.Start;
end;

{ TksInputListCheckBoxItem }

procedure TksInputListCheckBoxItem.CheckBoxChange(Sender: TObject);
begin
  if Assigned(FksInputList.OnItemCheckBoxChanged) then
    FksInputList.OnItemCheckBoxChanged(FksInputList, Self, ID, CheckBox.IsChecked);
end;

procedure TksInputListCheckBoxItem.ClickControl;
begin
  inherited;
  CheckBox.IsChecked := not CheckBox.IsChecked;
end;


function TksInputListCheckBoxItem.CreateControl: TPresentedControl;
begin
  Result := TCheckBox.Create(nil);
end;

function TksInputListCheckBoxItem.GetCheckBox: TCheckBox;
begin
  Result := (FControl as TCheckBox);
  (Result as TCheckBox).Width := 24;
  (Result as TCheckBox).Height := 24;
end;

class function TksInputListCheckBoxItem.GetClassID: string;
begin
  Result := '{07EC5B74-F220-45DD-BDC0-D4D686058C7C}';
end;

function TksInputListCheckBoxItem.GetValue: string;
begin
  Result := BoolToStr(CheckBox.IsChecked, True);
end;

procedure TksInputListCheckBoxItem.Reset;
begin
  inherited;
  CheckBox.IsChecked := False;
end;

procedure TksInputListCheckBoxItem.SetValue(const AValue: string);
begin
  inherited;
  CheckBox.IsChecked := StrToBool(AValue);
end;

{ TksInputListButtonItem }

procedure TksInputListButtonItem.ClickControl;
begin
  inherited;
  (FControl as TButton).IsPressed := True;
end;



function TksInputListButtonItem.CreateControl: TPresentedControl;
begin
  Result := TButton.Create(nil);
  (Result as TButton).StyleLookup := 'listitembutton';
  (Result as TButton).Height := 32;
  (Result as TButton).OnClick := DoButtonClick;
end;

procedure TksInputListButtonItem.DoButtonClick(Sender: TObject);
begin
  if Assigned(FksInputList.OnItemButtonClick) then
    FksInputList.OnItemButtonClick(FksInputList, Self, ID);
end;

function TksInputListButtonItem.GetButton: TButton;
begin
  Result := (FControl as TButton);
end;

class function TksInputListButtonItem.GetClassID: string;
begin
  Result := '{D7E87C25-E018-41F0-B85D-AA6B690C36CB}';
end;

procedure TksInputListButtonItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;
  Button.Text := AJson.Values['button_text'].Value;
  Button.Width := StrToFloat(AJson.Values['button_width'].Value);
end;

procedure TksInputListButtonItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('button_text', Button.Text);
  AJson.AddPair('button_width', FloatToStr(Button.Width));
end;

{ TksInputListTrackBarItem }

constructor TksInputListTrackBarItem.Create(AInputList: TksInputList);
begin
  inherited;
  TrackBar.Width := 200;
end;

function TksInputListTrackBarItem.CreateControl: TPresentedControl;
begin
  Result := TksTrackBar.Create(nil);
  (Result as TksTrackBar).ApplyStyle;
end;

class function TksInputListTrackBarItem.GetClassID: string;
begin
  Result := '{CBBF6D98-CCBD-4FCF-9AF4-6DE99E9E2362}'
end;

function TksInputListTrackBarItem.GetTrackBar: TksTrackBar;
begin
  Result := (FControl as TksTrackBar);
end;

function TksInputListTrackBarItem.GetValue: string;
begin
  Result := FloatToStr(TrackBar.Value);
end;

procedure TksInputListTrackBarItem.PaintControl(ACanvas: TCanvas);
var
  AThumbRect: TRectF;
begin
  inherited;
  AThumbRect := TrackBar.GetThumbRect;
  if TrackBar.Thumb <> nil then
  begin
    if TrackBar.Thumb.StyleState <> TStyleState.Applied then
      TrackBar.Thumb.ApplyStyleLookup;

    TrackBar.Thumb.PaintTo(ACanvas, AThumbRect);
  end;
end;

procedure TksInputListTrackBarItem.Reset;
begin
  inherited;
  TrackBar.Value := 0;
end;

procedure TksInputListTrackBarItem.SetValue(const AValue: string);
begin
  inherited;
  TrackBar.Value := StrToFloatDef(AValue, 0);
end;

procedure TksInputListTrackBarItem.TrackBarChange(Sender: TObject);
begin
  if Assigned(FksInputList.OnItemTrackBarChange) then
    FksInputList.OnItemTrackBarChange(FksInputList, Self, ID, StrToFloatDef(Value, 0));
end;

{ TksInputListSelectorItem }

constructor TksInputListSelectorItem.Create(AInputList: TksInputList);
begin
  inherited;
  FCombo := TComboBox.Create(nil);
  TempForm.AddObject(FCombo);
  FItems := TStringList.Create;
  FShowSelection := True;
  FCombo.OnChange := DoSelectorChanged;
end;

destructor TksInputListSelectorItem.Destroy;
begin
  FItems.Free;
  FCombo.Free;
  inherited;
end;

procedure TksInputListSelectorItem.DoSelectorChanged(Sender: TObject);
begin
  if FCombo.ItemIndex > -1 then
    Value := FCombo.Items[FCombo.ItemIndex];
  if Assigned(FksInputList.OnSelectorItemSelected) then
    FksInputList.OnSelectorItemSelected(FksInputList, Self, ID, Value);
end;

class function TksInputListSelectorItem.GetClassID: string;
begin
  Result := '{D5BB3373-99AB-4BB4-A478-96EAAC0AD091}';
end;

function TksInputListSelectorItem.GetValue: string;
begin
  Result := FValue;
end;

procedure TksInputListSelectorItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;
  FItems.CommaText := AJson.Values['items'].Value;
end;

procedure TksInputListSelectorItem.MouseUp(ATapEvent: Boolean);
begin
  if (FMouseDown) and (ATapEvent) then
  begin
    FCombo.OnChange := nil;
    FCombo.Items.Assign(FItems);
    FCombo.ItemIndex := FCombo.Items.IndexOf(FValue);
    FCombo.DropDown;
    FCombo.OnChange := DoSelectorChanged;
  end;
  inherited;
end;

procedure TksInputListSelectorItem.Reset;
begin
  inherited;
  Value := '';
end;

procedure TksInputListSelectorItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('items', FItems.CommaText);
end;

procedure TksInputListSelectorItem.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TksInputListSelectorItem.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FDetail := Value;
    FValue := Value;
    Changed;
  end;
end;

{ TInputListAccessoryImages }

constructor TInputListAccessoryImages.Create;
begin
  inherited Create(True);
  Add(TInputListAccessoryImage.Create(atMore, GetAccessoryFromResource(['listviewstyle','accessorymore'])));
  Add(TInputListAccessoryImage.Create(atCheckmark, GetAccessoryFromResource(['listviewstyle','accessorycheckmark'])));
  Add(TInputListAccessoryImage.Create(atDetail, GetAccessoryFromResource(['listviewstyle','accessorydetail'])));

end;

function TInputListAccessoryImages.GetItemByAccessory(AType: TksInputAccessoryType): TInputListAccessoryImage;
var
  AItem: TInputListAccessoryImage;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.AccessoryType = AType then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

{ TInputListAccessoryImage }

constructor TInputListAccessoryImage.Create(AType: TksInputAccessoryType; ABmp: TBitmap);
begin
  inherited Create;
  FAccessoryType := AType;
  FBitmap := ABmp;
end;

destructor TInputListAccessoryImage.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

{ TksInputListSeperator }


procedure TksInputListSeperator.DrawToCanvas(ACanvas: TCanvas);
begin
  FBackground := claNull;
  inherited;
end;

class function TksInputListSeperator.GetClassID: string;
begin
  Result := '{7DAC9C9A-3222-418B-A4B1-2EB33EC99466}';
end;

function TksInputListSeperator.GetValue: string;
begin
  Result := '';
end;

{ TksInputListItem }

class function TksInputListItem.GetClassID: string;
begin
  Result := '{4457C117-E5E0-41F3-9EC9-71BBB30C198D}';
end;

initialization

  TempForm := nil;
  AAccessoriesList := TInputListAccessoryImages.Create;

finalization

  TempForm.Free;
  AAccessoriesList.Free;

end.
