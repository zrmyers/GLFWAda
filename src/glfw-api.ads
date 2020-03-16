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

private package Glfw.Api is

    -- Return Codes that can be passed back from GLFW operations
    type Enum_Return_Codes is (
        NO_ERROR,
        NOT_INITIALIZED,
        NO_CURRENT_CONTEXT,
        INVALID_ENUM,
        INVALID_VALUE,
        OUT_OF_MEMORY,
        API_UNAVAILABLE,
        VERSION_UNAVAILABLE,
        PLATFORM_ERROR,
        FORMAT_UNAVAILABLE,
        NO_WINDOW_CONTEXT
    );
    -- Values to use for Return_Codes enumeration.
    for Enum_Return_Codes use (
        NO_ERROR            => 16#00000000#,
        NOT_INITIALIZED     => 16#00010001#,
        NO_CURRENT_CONTEXT  => 16#00010002#,
        INVALID_ENUM        => 16#00010003#,
        INVALID_VALUE       => 16#00010004#,
        OUT_OF_MEMORY       => 16#00010005#,
        API_UNAVAILABLE     => 16#00010006#,
        VERSION_UNAVAILABLE => 16#00010007#,
        PLATFORM_ERROR      => 16#00010008#,
        FORMAT_UNAVAILABLE  => 16#00010009#,
        NO_WINDOW_CONTEXT   => 16#0001000A#
    );
    for Enum_Return_Codes'Size use Interfaces.C.int'Size;
    
    -- Return codes related to initializing and terminating GLFW on the current
    -- platform.
    subtype Enum_Platform_Return_Codes is Enum_Return_Codes 
        with Static_Predicate => Enum_Platform_Return_Codes in 
            NO_ERROR | 
            PLATFORM_ERROR;
    
    -- Return codes related to initializing a GLFW Window.
    subtype Enum_Window_Init_Return_Codes is Enum_Return_Codes 
        with Static_Predicate => Enum_Window_Init_Return_Codes in
            NO_ERROR |
            NOT_INITIALIZED |
            INVALID_ENUM |
            INVALID_VALUE |
            API_UNAVAILABLE |
            VERSION_UNAVAILABLE |
            FORMAT_UNAVAILABLE |
            PLATFORM_ERROR;
            
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
                   
                             
end Glfw.Api;
