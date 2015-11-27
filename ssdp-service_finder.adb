with SSDP.Utils;

package body SSDP.Service_Finder is
   use SSDP.Utils;

   M_Search_Star_Line: constant String := "M-SEARCH * HTTP/1.1";

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return Device_Type is
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      return (To_US(Service_Type), To_US(Universal_Serial_Number));
   end Initialize_Device;

   procedure Set_Universal_Serial_Number(Device: in out Device_Type;
					 Universal_Serial_Number: in String) is
   begin
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);
   end Set_Universal_Serial_Number;

   procedure Set_Service_Type(Device: in out Device_Type;
			      Service_Type: in String) is
   begin
      Device.Service_Type := To_US(Service_Type);
   end Set_Service_Type;

   function Get_Universal_Serial_Number(Device: in Device_Type) return String is
   begin
      return To_String(Device.Universal_Serial_Number);
   end Get_Universal_Serial_Number;

   function Get_Service_Type(Device: in Device_Type) return String is
   begin
      return To_String(Device.Service_Type);
   end Get_Service_Type;

   procedure M_Search(Device: in Device_Type;
		      Other_Header: in Message_Header_Array) is
      Start_Line: constant String := M_Search_Star_Line & EOL;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type) is missing";
      end if;

      Send_Message(Create_Message(Start_Line & "ST: " &
				    To_String(Device.Service_Type) & EOL &
				    "USN: " &
				    To_String(Device.Universal_Serial_Number) &
				    EOL, Other_Header));
   end M_Search;

end SSDP.Service_Finder;
