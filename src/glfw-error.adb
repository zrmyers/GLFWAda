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
--------------------------------------------------------------------------------
-- This file provides an interface to Data Types and operations for raising
-- exceptions after detection of an error in GLFW.
--------------------------------------------------------------------------------
with Glfw.Api;
with Interfaces.C.Strings;

package body Glfw.Error is

    procedure Raise_If_Present is
      
        error_message : Interfaces.C.Strings.chars_ptr;
        return_code : Enum_Return_Codes;
    begin
        
        return_code := Api.glfwGetError(message => error_message);
        case return_code is
        
            -- No Errors Occurred, yay!
            when NO_ERROR =>
                null;
                
            -- Platform error occurred, printing error message.
            when PLATFORM_ERROR =>
                raise Exceptions.PLATFORM_ERROR 
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                        
            when NOT_INITIALIZED =>
                raise Exceptions.NOT_INITIALIZED
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when NO_CURRENT_CONTEXT =>
                raise Exceptions.NO_CURRENT_CONTEXT
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when INVALID_ENUM =>
                raise Exceptions.INVALID_ENUM
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when INVALID_VALUE =>
                raise Exceptions.INVALID_VALUE
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when OUT_OF_MEMORY =>
                raise Exceptions.OUT_OF_MEMORY
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when API_UNAVAILABLE =>
                raise Exceptions.API_UNAVAILABLE
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when VERSION_UNAVAILABLE =>
                raise Exceptions.VERSION_UNAVAILABLE
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                                      
            when FORMAT_UNAVAILABLE =>
                raise Exceptions.FORMAT_UNAVAILABLE
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
            when NO_WINDOW_CONTEXT =>
                raise Exceptions.NO_WINDOW_CONTEXT
                    with Interfaces.C.Strings.Value(
                        Item => error_message);
                    
        end case;
                        
    end Raise_If_Present;
    
end Glfw.Error;
