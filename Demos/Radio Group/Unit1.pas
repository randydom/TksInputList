unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  ksInputList, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    ksInputList1: TksInputList;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ksInputList1.Items.AddRadioItem('ITEM1', nil, 'RADIOGROUP', 'Item 1', True);
  ksInputList1.Items.AddRadioItem('ITEM2', nil, 'RADIOGROUP', 'Item 2', False);
  ksInputList1.Items.AddRadioItem('ITEM3', nil, 'RADIOGROUP', 'Item 3', False);
  ksInputList1.Items.AddRadioItem('ITEM4', nil, 'RADIOGROUP', 'Item 4', False);
end;

end.
