package Pack.OS
is

   --  Execute given command. Raises a Command_Failed exception if the command
   --  exit status is non-zero or the command was not found.
   procedure Execute (Command : String);

   Command_Failed : exception;

end Pack.OS;
