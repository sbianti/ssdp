with Ada.Text_IO;
with SSDP.Utils;

package body SSDP.Service_Finder is
   use SSDP.Utils;

   M_Search_Star_Line: constant String := "M-SEARCH * HTTP/1.1";

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return Finder_Device_Type is
      Device: Finder_Device_Type;
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      Device.Connection := Activate_Connection;
      Device.Service_Type := To_US(Service_Type);
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);

      return Device;
   end Initialize_Device;

   procedure Set_Universal_Serial_Number(Device: in out Finder_Device_Type;
					 Universal_Serial_Number: in String) is
   begin
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);
   end Set_Universal_Serial_Number;

   procedure Set_Service_Type(Device: in out Finder_Device_Type;
			      Service_Type: in String) is
   begin
      Device.Service_Type := To_US(Service_Type);
   end Set_Service_Type;

   function Get_Universal_Serial_Number(Device: in Finder_Device_Type)
				       return String is
   begin
      return To_String(Device.Universal_Serial_Number);
   end Get_Universal_Serial_Number;

   function Get_Service_Type(Device: in Finder_Device_Type) return String is
   begin
      return To_String(Device.Service_Type);
   end Get_Service_Type;

   procedure M_Search(Device: in out Finder_Device_Type;
		      Other_Header: in Message_Header_Array) is
      Start_Line: constant String := M_Search_Star_Line & EOL;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type) is missing";
      end if;

      Send_Message(Device_Type(Device),
		   Create_Message(Start_Line & "ST: " &
				    To_String(Device.Service_Type) & EOL &
				    "USN: " &
				    To_String(Device.Universal_Serial_Number) &
				    EOL, Other_Header));
   end M_Search;

   task Listener is
      entry Start(Dev: in out Finder_Device_Type);
   end Listener;

   task body Listener is
      Device: Finder_Device_Type;
   begin
      accept Start(Dev: in out Finder_Device_Type) do
	 Device := Dev;
	 if Device.Connection.Is_Down then
	    Device.Connection := Activate_Connection;
	    Device.Connection.Is_Listening := True;
	 end if;
      end Start;

      loop
	 declare
	    use Ada.Text_IO;
	    Message: String := String'Input(Device.Connection.Channel);
	 begin
	    Put_Line("[[" & Message & "]]");
	 end;
      end loop;
   end Listener;

   procedure Start_Listening(Device: in out Finder_Device_Type) is
   begin
      if not Device.Connection.Is_Listening then
	 Listener.Start(Device);
	 Device.Connection.Is_Listening := True;
      end if;
   end Start_Listening;

   procedure Stop_Listening(Device: in out Finder_Device_Type) is
   begin
      if Device.Connection.Is_Listening then
	 abort Listener;
	 Device.Connection.Is_Listening := False;
      end if;
   end Stop_Listening;

end SSDP.Service_Finder;
