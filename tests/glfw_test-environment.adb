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

    glfw_return_code : Glfw.Enum_Return_Codes := Glfw.NO_ERROR;
    
begin
    Ada.Text_IO.Put_Line("Initializing GLFW");
    
    -- Initialize GLFW
    glfw_return_code := Glfw.Init;
   
    if 
        not (glfw_return_code = Glfw.NO_ERROR)
    then
        Ada.Text_IO.Put_Line(" Glfw.Init Failed:");
        Ada.Text_IO.Put(Glfw.Enum_Return_Codes'Image(glfw_return_code));
    end if;
    
    
    -- Set Window Hints
    Glfw.Set_Window_Hints;
    
	-- Initialize Window
	Glfw.Create_Window;
   
   -- Main Loop
    while 
        not Glfw.Window_Should_Close
    loop
   
        -- Poll for Glfw events
        Glfw.PollEvents;
    end loop;
   
    Glfw.Destroy_Window;
   
    -- Shut down the GLFW instance
    glfw_return_code := Glfw.Shutdown;
    
    if 
        not (glfw_return_code = Glfw.NO_ERROR)
    then
        Ada.Text_IO.Put_Line(" Glfw.Shutdown Failed:");
        Ada.Text_IO.Put(Glfw.Return_Codes'Image(glfw_return_code));
    end if;
    
end Glfw_Test.Environment;

