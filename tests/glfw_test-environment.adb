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

procedure Glfw_Test.Environment is

    -- The default window configuration is used, with no client API specified.
    window_hints : Glfw.Record_Window_Hints := (
        Client_Api => Glfw.NO_API,
        others     => <>
    );

    window_handle : Glfw.Glfw_Window := Glfw.No_Window;

    required_extension_names : Glfw.Glfw_String_Vector;
begin
    Ada.Text_IO.Put_Line("Initializing GLFW");

    -- Initialize GLFW
    Glfw.Platform_Init;

    Ada.Text_IO.Put_Line("Initializing Window");

    -- Set window hints
    Glfw.Window_Set_Hints(
        hints => window_hints);

    -- Initialize a GLFW Window
    window_handle :=
        Glfw.Window_Create(
            width         => 1024,
            height        => 768,
            title         => "Hello World!");

    Ada.Text_IO.Put_Line("Running GLFW Main Loop");

    Glfw.Get_Required_Instance_Extensions(required_extension_names);

    for name of required_extension_names loop
        Ada.Text_IO.Put_Line("Required_Extension: " & Glfw.To_String(name));
    end loop;
    -- Main Loop
    loop

        ------------------------------------------------------------------------
        -- This loop will run forever until the user closes the window or some
        -- kind of exception occurs, which causes the program to begin handling
        -- the exception.
        ------------------------------------------------------------------------
        pragma Warnings (Off, "variable ""window_handle"" is not modified in loop body");
        pragma Warnings (Off, "possible infinite loop");

        exit when Glfw.Window_Should_Close(window_handle => window_handle);

        pragma Warnings (On, "variable ""window_handle"" is not modified in loop body");
        pragma Warnings (On, "possible infinite loop");

        -- Poll for Glfw events
        Glfw.Platform_Process_Events;
    end loop;

    Ada.Text_IO.Put_Line("Destroying Window");

    Glfw.Window_Destroy(window_handle => window_handle);

    Ada.Text_IO.Put_Line("Shutting Down GLFW");

    -- Shut down the GLFW instance
    Glfw.Platform_Shutdown;


end Glfw_Test.Environment;
