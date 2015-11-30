with Ada.Text_IO;
with SSDP.Service_Provider;

procedure Test_Service_Provider is
   use Ada.Text_IO, SSDP.Service_Provider;

   Notify_Header: SSDP.Message_Header_Array := (To_US("Affaires: dentifrice"),
						To_US("DIY: courses:rien"));

   UUID: constant String := "822ccbbf-3aa6-44c2-80ef-0307f9673521";

   Null_Header: SSDP.Message_Header_Array(1..0);

   Device: Service_Provider_Device_Type :=
     Initialize_Device(Service_Type => "chauffage:central",
		       Universal_Serial_Number => UUID,
		       Location => "",
		       AL => "<http://halsensortester.mp.intel.com>",
		       Cache_Control => "max-age = 600",
		       Expires => "");

   Str: String(1..10);
   Lg: Natural;
begin
   Start_Listening;
   delay 0.2;

   Put_Line("Envoie du Notify:");
   Notify_Alive(Device, Notify_Header);

   Get_Line(Str, Lg);
   Put_Line("Au revoir !");
   Notify_Bye_Bye(Device);

   Stop_Listening;
end Test_Service_Provider;
