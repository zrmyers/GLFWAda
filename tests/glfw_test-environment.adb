--------------------------------------------------------------------------------
-- MIT License
--
-- Copyright (c) 2020 Zane Myers
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--------------------------------------------------------------------------------
with Ada.Text_IO;
With Glfw;

use type Glfw.Enum_Return_Codes;

procedure Glfw_Test.Environment is

    -- The default window configuration is used, with no client API specified.
    window_config : Glfw.Record_Window_Configuration := (
        Client_Api => Glfw.NO_API,
        others     => <>
    );
    
    glfw_return_code : Glfw.Enum_Return_Codes := Glfw.NO_ERROR;
    
    window_id : Glfw.Window_Id := Glfw.NONE;
    
begin
    Ada.Text_IO.Put_Line("Initializing GLFW");
    
    -- Initialize GLFW
    glfw_return_code := Glfw.Platform_Init;
   
    if 
        not (glfw_return_code = Glfw.NO_ERROR)
    then
        Ada.Text_IO.Put_Line(" Glfw.Init Failed:");
        Ada.Text_IO.Put(Glfw.Enum_Return_Codes'Image(glfw_return_code));
    end if;
    
    -- Initialize a GLFW Window
    glfw_return_code := 
        Glfw.Window_Init(
            width         => 1024,
            height        => 768,
            title         => "Hello World!",
            window_config => window_config,
            window        => window_id);
   
   if 
       not (glfw_return_code = Glfw.NO_ERROR)
   then
       Ada.Text_IO.Put_Line(" Glfw.Window_Init Failed:");
       Ada.Text_IO.Put(Glfw.Enum_Return_Codes'Image(glfw_return_code));
   end if;
   
   -- Main Loop
    loop 
        ------------------------------------------------------------------------
        -- This loop will run forever until the user closes the window or some
        -- kind of exception occurs, which causes the program to begin handling
        -- the exception.
        ------------------------------------------------------------------------
        pragma Warnings (Off, "variable ""window_id"" is not modified in loop body");
        pragma Warnings (Off, "possible infinite loop");
        
        exit when Glfw.Window_Should_Close(window => window_id);
        
        pragma Warnings (On, "variable ""window_id"" is not modified in loop body");
        pragma Warnings (On, "possible infinite loop");
        
        -- Poll for Glfw events
        Glfw.Platform_Process_Events;
    end loop;
   
    Glfw.Window_Destroy(window => window_id);
   
    -- Shut down the GLFW instance
    glfw_return_code := Glfw.Platform_Shutdown;
    
    if 
        not (glfw_return_code = Glfw.NO_ERROR)
    then
        Ada.Text_IO.Put_Line(" Glfw.Shutdown Failed:");
        Ada.Text_IO.Put(Glfw.Enum_Return_Codes'Image(glfw_return_code));
    end if;
    
end Glfw_Test.Environment;

