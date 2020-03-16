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
with System;
with Glfw.Error;

private package Glfw.Api is    
            
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
                   
                   
end Glfw.Api;
