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

   type Command_Name_Type is (Discover, Sleep, Alive, Bye_Bye);

   -- Restricted command_name types:
   type Finder_Command_Name_Type is new Command_Name_Type range Discover..Sleep;
   type Provider_Command_Name_Type is new Command_Name_Type range Sleep..Bye_Bye;

   Header_Malformed,
   Not_An_SSDP_Message,
   SSDP_Message_Malformed: exception;

private

   type Device_Type is tagged record
      Service_Type,
      Universal_Serial_Number: Unbounded_String;
   end record;

end SSDP;
