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
with Glfw.Api;
with Glfw.Error;

package body Glfw is
    
    ----------------------------------------------------------------------------
    -- Platform Functions
    ----------------------------------------------------------------------------


    procedure Platform_Init 
    is
        is_successful : Glfw_Bool := FALSE;
    begin
        
        is_successful := Api.glfwInit;
        
        if not is_successful then
        
            -- A platform error must have occurred.
            Error.Raise_If_Present;
            
        end if;
        
    end Platform_Init;
    
    
    ----------------------------------------------------------------------------


    procedure Platform_Shutdown is
    begin
        Error.Raise_If_Present;
    end Platform_Shutdown;
    
    
    ----------------------------------------------------------------------------


    procedure Platform_Process_Events is 
    begin 
        Error.Raise_If_Present;
    end Platform_Process_Events;
    
    
    ----------------------------------------------------------------------------
    -- Window Functions
    ----------------------------------------------------------------------------
    
    
    procedure Window_Init
        (
            width         : in     Window_Dimmension;
            height        : in     Window_Dimmension;
            title         : in     String;
            monitor       : in     Monitor_Id := NONE;
            share         : in     Window_Id  := NONE;
            window_config : in     Record_Window_Configuration;
            window        : out    Window_Id
        ) 
    is     
        
    begin
    
       Error.Raise_If_Present;
    end Window_Init;
    
    
    ----------------------------------------------------------------------------


    function Window_Should_Close
        (
            window : in     Window_Id
        )
        return Boolean 
    is
    
    begin
        Error.Raise_If_Present;
       
        return TRUE;
    end;
    
    ----------------------------------------------------------------------------


    procedure Window_Destroy
        (
            window : in     Window_Id
        ) 
    is
    begin
       Error.Raise_If_Present;
    end;
        
    
    ----------------------------------------------------------------------------
    -- Local Package Implementations
    ----------------------------------------------------------------------------
    package body Error is
    
        procedure Raise_If_Present is
          
            error_message : Interfaces.C.char_array;
            return_code : Enum_Return_Codes;
        begin
            
            return_code := Api.glfwGetError(message => error_message);
            case return_code is
                when NO_ERROR =>
                    null;
                when PLATFORM_ERROR =>
                    raise Exceptions.PLATFORM_ERROR 
                        with Interfaces.C.To_Ada(
                            Item     => error_message,
                            Trim_Nul => True);
            end case;
                            
        end Raise_If_Present;
        
    end Error;
    
end Glfw;
