package SSDP.Service_Provider is

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type Service_Provider_Device_Type is new Device_Type with private;

   function Initialize_Device(Service_Type, Universal_Serial_Number,
				Location, AL, -- only one is required
				Cache_Control, Expires: String) -- dito
			     return Service_Provider_Device_Type;

   procedure M_Search_Response(Device: in out Service_Provider_Device_Type;
			       USN_Requester: in String;
			       Other_Headers: in Message_Header_Array);

   procedure Notify_Alive(Device: in out Service_Provider_Device_Type;
			  Other_Headers: in Message_Header_Array);

   procedure Notify_Bye_Bye(Device: in out Service_Provider_Device_Type);

   procedure Start_Listening;

   procedure Stop_Listening;

private
   type Service_Provider_Device_Type is new Device_Type with record
      Location, AL, -- only one is required
      Cache_Control, Expires: Unbounded_String; -- dito
   end record;
end SSDP.Service_Provider;
