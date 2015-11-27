with SSDP.Utils;

package body SSDP.Service_Provider is
   use SSDP.Utils;

   Notify_Line: constant String := "NOTIFY * HTTP/1.1" & EOL;
   Status_Line: constant String := "HTTP/1.1 200 OK" & EOL;

   function Initialize_Device(Service_Type, Universal_Serial_Number,
				Location, AL, -- only one is required
				Cache_Control, Expires: String) -- dito
			     return Service_Provider_Device_Type is
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      if Location = "" and AL = "" then
	 raise Header_Malformed
	   with "Location or AL should be set";
      end if;

      if Cache_Control = "" and Expires = "" then
	 raise Header_Malformed
	   with "Cache_Control or Expires should be set";
      end if;

      return (To_US(Service_Type), To_US(Universal_Serial_Number),
	      Activate_Connection,
	      To_US(Location), To_US(AL),
	      To_US(Cache_Control), To_US(Expires));
   end Initialize_Device;

   procedure M_Search_Response(Device: in out Service_Provider_Device_Type;
			       USN_Requester: in String;
			       Other_Headers: in Message_Header_Array) is
      Required_Part: Unbounded_String;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      if Device.Cache_Control = "" or Device.Expires = "" then
	 raise Header_Malformed
	   with "Cache-control or Expires missing (at least one is required)";
      end if;

      if USN_Requester = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type of the requester) is missing";
      end if;

      Required_Part := To_US(Status_Line & "S: " & USN_Requester & EOL &
			       "USN: ") & Device.Universal_Serial_Number &
	To_US(EOL & "ST: ") & Device.Service_Type & To_US(EOL);

      if Device.Cache_Control /= "" then
	 Append(Required_Part, To_US("Cache-Control: ") &
		  Device.Cache_Control & To_US(EOL));
      end if;

      if Device.Expires /= "" then
	 Append(Required_Part, To_US("Expires: ") &
		  Device.Expires & To_US(EOL));
      end if;

      Send_Message(Device_Type(Device),
		   Create_Message(To_String(Required_Part), Other_Headers));
   end M_Search_Response;

   procedure Notify_Alive(Device: in out Service_Provider_Device_Type;
			  Other_Headers: in Message_Header_Array) is
      Required_Part: Unbounded_String;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «NT» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      if Device.Location = "" and Device.AL = "" then raise Header_Malformed
	with "Both headers «Location» and «AL» are missing," &
	" at least one is required";
      end if;

      if Device.Cache_Control = "" and Device.Expires = "" then
	 raise Header_Malformed
	   with "Both headers «Cache-Control» and «Expires» are missing," &
	   " at least one is required";
      end if;

      Required_Part := To_US(Notify_Line & "NT: ") & Device.Service_Type &
	To_US(EOL & "USN: ") & Device.Universal_Serial_Number &
	To_US(EOL & "NTS: ssdp:alive" & EOL);

      if Device.Location /= "" then
	 Append(Required_Part, To_US("Location: ") &
		  Device.Location & To_US(EOL));
      end if;

      if Device.AL /= "" then
	 Append(Required_Part, To_US("AL: ") & Device.AL & To_US(EOL));
      end if;

      if Device.Cache_Control /= "" then
	 Append(Required_Part, To_US("Cache-Control: ") &
		  Device.Cache_Control & To_US(EOL));
      end if;

      if Device.Expires /= "" then
	 Required_Part := Required_Part & To_US("Expires: ") &
	   Device.Expires & To_US(EOL);
      end if;

      Send_Message(Device_Type(Device),
		   Create_Message(To_String(Required_Part), Other_Headers));
   end Notify_Alive;

   procedure Notify_Bye_Bye(Device: in out Service_Provider_Device_Type) is
      Start_Line: constant String := Notify_Line;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «NT» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then
	 raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      Send_Message(Device_Type(Device),
		   Start_Line & "NT: " & To_String(Device.Service_Type) & EOL &
		     "USN: " & To_String(Device.Universal_Serial_Number) & EOL &
		     "NTS: ssdp:byebye" & EOL & EOL);
   end Notify_Bye_Bye;

end Ssdp.Service_Provider;
