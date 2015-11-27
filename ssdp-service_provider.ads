package SSDP.Service_Provider is

   function To_US(Str: String) return Unbounded_String
     renames To_Unbounded_String;

   type Device_Type is private;

   function Initialize_Device(Service_Type, Universal_Serial_Number,
				Location, AL, -- only one is required
				Cache_Control, Expires: String) -- dito
			     return Device_Type;

   procedure M_Search_Response(Device: in Device_Type;
			       USN_Requester: in String;
			       Other_Headers: in Message_Header_Array);

   procedure Notify_Alive(Device: in Device_Type;
			  Other_Headers: in Message_Header_Array);

   procedure Notify_Bye_Bye(Device: in Device_Type);

private
   type Device_Type is record
      Service_Type,
      Universal_Serial_Number,
      Location, AL, -- only one is required
      Cache_Control, Expires: Unbounded_String; -- dito
   end record;
end SSDP.Service_Provider;
