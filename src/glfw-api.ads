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
with Interfaces.C;
with Interfaces.C.Strings;
with Glfw.Error;
with Glfw.Window_Hints;

private package Glfw.Api is    
            
    ----------------------------------------------------------------------------
    -- Platform Level Functions
    ----------------------------------------------------------------------------
    -- Import the glfwInit() function from the GLFW C library.
    function glfwInit return Glfw_Bool;
    pragma Import (Convention    => C,
                   Entity        => glfwInit,
                   External_Name => "glfwInit");
                   
                   
    -- Import the glfwTerminate() function from the GLFW C library.
    procedure glfwTerminate;
    pragma Import (Convention    => C,
                   Entity        => glfwTerminate,
                   External_Name => "glfwTerminate"); 
                   
                   
    -- Import the glfwGetError() function from the GLFW C library.
    function glfwGetError
        (
            message :    out Interfaces.C.Strings.chars_ptr
        )
        return Error.Enum_Return_Codes;
    pragma Import (Convention    => C,
                   Entity        => glfwGetError,
                   External_Name => "glfwGetError");
                   
         
    -- Import the glfwPollEvents() function from the GLFW C library
    procedure glfwPollEvents;
    pragma Import (Convention    => C,
                   Entity        => glfwPollEvents,
                   External_Name => "glfwPollEvents");
            
            
    ----------------------------------------------------------------------------
    -- Window Level Functions
    ----------------------------------------------------------------------------
    -- Import the glfwWindowHint() function from the GLFW C library
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Glfw_Bool
        );
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Glfw_Int
        );
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Enum_Client_Api
        );
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Enum_Context_Api
        );
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Enum_Context_Robustness
        );
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Enum_Context_Release_Behavior
        );
    procedure glfwWindowHint
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Glfw.Enum_OpenGl_Profile
        );
    pragma Import (Convention    => C,
                   Entity        => glfwWindowHint,
                   External_Name => "glfwWindowHint");
                   
    -- Import the glfwWindHintString() function from the GLFW Library.
    procedure glfwWindowHintString
        (
            window_hint       : in     Glfw.Window_Hints.Enum_Window_Hints;
            window_hint_value : in     Interfaces.C.Strings.chars_ptr
        );
    pragma Import (Convention    => C,
                   Entity        => glfwWindowHintString,
                   External_Name => "glfwWindowHintString");
    
    -- Import the glfwCreateWindow() function from the GLFW Library.
    function glfwCreateWindow
        (
            width   : in     Window_Dimmension;
            height  : in     Window_Dimmension;
            title   : in     Interfaces.C.Strings.chars_ptr;
            monitor : in     Glfw_Monitor;
            window  : in     Glfw_Window
        )
        return Glfw_Window;
    pragma Import (Convention    => C,
                   Entity        => glfwCreateWindow,
                   External_Name => "glfwCreateWindow");
                   
    -- Import the glfwWindowShouldClose() function from the GLFW Library.
    function glfwWindowShouldClose
        (
            window : in     Glfw_Window
        )
        return Glfw_Bool;
    pragma Import (Convention    => C,
                   Entity        => glfwWindowShouldClose,
                   External_Name => "glfwWindowShouldClose");
                   
    -- Import the glfwDestroyWindow() function from the GLFW Library.
    procedure glfwDestroyWindow
        (
            window : in     Glfw_Window
        );
    pragma Import (Convention    => C,
                   Entity        => glfwDestroyWindow,
                   External_Name => "glfwDestroyWindow");
end Glfw.Api;
