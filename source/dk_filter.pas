unit DK_Filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, DK_StrUtils, UFilterImages;

const
  DELAY_MILLISECONDS_DEFAULT = 1;

type
  TFilterEvent = procedure(const AFilterString: String) of object;

  { TDKFilter }

  TDKFilter = class(TForm)
    FilterButton: TSpeedButton;
    FilterEdit: TEdit;
    FilterLabel: TLabel;
    FilterTimer: TTimer;
    procedure FilterButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OnFilterChange: TFilterEvent;
    CanApplyFilter: Boolean;
    CanStartTimer: Boolean;
    Images: TFilterImages;

    function GetFilterEnabled: Boolean;
    function GetFilterString: String;

    procedure SetFilterEnabled(const AValue: Boolean);
    procedure SetFilterString(const AValue: String);

    procedure DoChange;
  public
    procedure Focus;
    procedure Clear(const ADoOnChangeEvent: Boolean = True);

    property FilterEnabled: Boolean read GetFilterEnabled write SetFilterEnabled;
    property FilterString: String read GetFilterString write SetFilterString;
  end;

var
  DKFilter: TDKFilter;

  function DKFilterCreate(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AWidth: Integer = -1;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT;
        const AOnFilterKeyDown: TKeyEvent = nil): TDKFilter;

implementation

{$R *.lfm}

function DKFilterCreate(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AWidth: Integer = -1;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT;
        const AOnFilterKeyDown: TKeyEvent = nil): TDKFilter;
begin
  Result:= TDKFilter.Create(APanel);
  Result.Parent:= APanel;
  Result.Parent.Caption:= EmptyStr;
  if AWidth>0 then
  begin
    Result.Parent.AutoSize:= False;
    Result.Parent.Width:= AWidth;
  end
  else
    Result.Parent.AutoSize:= True;
  Result.FilterLabel.Caption:= ACaption;
  Result.OnFilterChange:= AOnFilterChange;
  if Assigned(AOnFilterKeyDown) then
    Result.FilterEdit.OnKeyDown:= AOnFilterKeyDown;
  Result.FilterTimer.Interval:= AUpdateDelayMilliSeconds;
  Result.Show;
end;

{ TDKFilter }

procedure TDKFilter.FormCreate(Sender: TObject);
begin
  Images:= TFilterImages.Create(nil);
  CanApplyFilter:= False;
  CanStartTimer:= True;
end;

procedure TDKFilter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Images);
end;

procedure TDKFilter.FormShow(Sender: TObject);
begin
  FilterButton.Height:= FilterEdit.Height;
  {$IFDEF WINDOWS}
  FilterButton.Height:= FilterButton.Height + 2;
  {$ENDIF}
  FilterButton.Width:= FilterButton.Height;
  FilterButton.Images:= Images.ForScreenPPI;
end;

function TDKFilter.GetFilterEnabled: Boolean;
begin
  Result:= FilterEdit.Enabled;
end;

function TDKFilter.GetFilterString: String;
begin
  Result:= FilterEdit.Text;
end;

procedure TDKFilter.SetFilterEnabled(const AValue: Boolean);
begin
  FilterEdit.Enabled:= AValue;
  if AValue then
    FilterButton.Enabled:= not SEmpty(FilterEdit.Text)
  else
    FilterButton.Enabled:= False;
end;

procedure TDKFilter.SetFilterString(const AValue: String);
begin
  if SSame(FilterEdit.Text, AValue) then Exit;
  FilterEdit.Text:= AValue;
  FilterTimer.Enabled:= False;
  DoChange;
end;

procedure TDKFilter.Focus;
begin
  FilterEdit.SetFocus;
end;

procedure TDKFilter.DoChange;
begin
  if Assigned(OnFilterChange) then
    OnFilterChange(SUpper(FilterEdit.Text));
end;

procedure TDKFilter.FilterTimerTimer(Sender: TObject);
begin
  FilterTimer.Enabled:= False;
  if not CanApplyFilter then Exit;
  DoChange;
end;

procedure TDKFilter.FilterEditChange(Sender: TObject);
begin
  FilterButton.Enabled:= not SEmpty(FilterEdit.Text);
  if not CanStartTimer then Exit;

  CanApplyFilter:= False;
  if FilterTimer.Enabled then
    FilterTimer.Enabled:= False;
  FilterTimer.Enabled:= True;
  CanApplyFilter:= True;
end;

procedure TDKFilter.Clear(const ADoOnChangeEvent: Boolean = True);
begin
  CanStartTimer:= False;
  FilterEdit.Text:= EmptyStr;
  CanStartTimer:= True;
  if ADoOnChangeEvent then DoChange;
end;

procedure TDKFilter.FilterButtonClick(Sender: TObject);
begin
  Clear;
end;

end.

