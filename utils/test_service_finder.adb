with Ada.Text_Io;
with SSDP.Service_Finder;

procedure Test_Service_Finder is
   use Ssdp.Service_Finder, Ada.Text_Io;

   Discover_Header: SSDP.Message_Header_Array := (To_US("Toto: inutile:pardon"),
						  To_US("Zak: important:rien"));

   UUID: constant String := "uuid:0dbcf247-96ca-4d58-b3de-a22cd083125b";

   Device: Finder_Device_Type := Initialize_Device("sncf:TGV", UUID);
begin
   Start_Listening(Device);
   delay 0.5;

   Put_Line("Envoie du Discover:");
   M_Search(Device, Discover_Header);
   delay 1.0;

   Put_Line("Envoie du deuxième Discover:");
   M_Search(Device, Discover_Header);

   delay 2.0;
   Stop_Listening(Device);
end Test_Service_Finder;
