unit UFilterImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DK_CtrlUtils;

type

  { TFilterImages }

  TFilterImages = class(TDataModule)
    PX16: TImageList;
    PX20: TImageList;
    PX24: TImageList;
    PX28: TImageList;
  private

  public
    function ForScreenPPI: TImageList;
  end;

var
  FilterImages: TFilterImages;

implementation

{$R *.lfm}

{ TFilterImages }

function TFilterImages.ForScreenPPI: TImageList;
begin
  Result:= ChooseImageListForScreenPPI(PX16, PX20, PX24, PX28);
end;

end.

