with Ada.Strings.Unbounded;

package SSDP is
   use Ada.Strings.Unbounded;

   type US_Array is array (Positive range <>) of Unbounded_String;
   subtype Location_Array is US_Array;
   subtype Message_Header_Array is US_Array;

   type Device_Type is tagged private;

   type Job_Procedure_Access is access procedure;

    ----------------------------------------------------------------------------
    --  SSDP messages are http 1.1 messages where:			      --
    --   ⋅service type URI represents the type of service (ex: refrigerator)  --
    --   ⋅USN is a URI which uniquely identifies a particular instance of a   --
    --  service, ex: upnp:uuid:bla:… for a device that has a UUID             --
    ----------------------------------------------------------------------------

   --  type SSDP_Record_Type(Location_Number: Natural) is record
   --     Service_Type_Uri: Unbounded_String;
   --     Unique_Service_Name_Uri: Unbounded_String;
   --     Expiration_Announcement: Calendar.Time;
   --     Location: Location_Array(1..Location_Number);
   --     ST_Header: Unbounded_String;
   --  end record;

   Header_Malformed,
   Not_An_SSDP_Message,
   SSDP_Message_Malformed: exception;

private

   type Device_Type is tagged record
      Service_Type,
      Universal_Serial_Number: Unbounded_String;
   end record;

end SSDP;
