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
with Glfw.Window_Hints;
with Interfaces.C.Strings;
use Interfaces.C;
with System; use System;
with Ada.Unchecked_Conversion;

package body Glfw is

    -- Renames
    Package CStrings renames Interfaces.C.Strings;


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


    procedure Platform_Shutdown
    is
    begin

        Api.glfwTerminate;

        Error.Raise_If_Present;
    end Platform_Shutdown;


    ----------------------------------------------------------------------------


    procedure Platform_Process_Events
    is
    begin

        Api.glfwPollEvents;

        Error.Raise_If_Present;
    end Platform_Process_Events;


    ----------------------------------------------------------------------------
    -- Window Functions
    ----------------------------------------------------------------------------


    procedure Window_Set_Hints
        (
            hints : Record_Window_Hints
        )
    is begin

        for Hint in Window_Hints.Enum_Window_Hints'Range loop

            case Hint is

                when Window_Hints.FOCUSED =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.FOCUSED);

                when Window_Hints.RESIZABLE =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.RESIZABLE);

                when Window_Hints.VISIBLE =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.VISIBLE);

                when Window_Hints.DECORATED =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.DECORATED);

                when Window_Hints.AUTO_ICONIFY =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.AUTO_ICONIFY);

                when Window_Hints.FLOATING =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.FLOATING);

                when Window_Hints.MAXIMIZED =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.MAXIMIZED);

                when Window_Hints.CENTER_CURSOR =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CENTER_CURSOR);

                when Window_Hints.TRANSPARENT_FRAMEBUFFER =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.TRANSPARENT_FRAMEBUFFER);

                when Window_Hints.FOCUS_ON_SHOW =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.FOCUS_ON_SHOW);

                when Window_Hints.RED_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.RED_BITS);

                when Window_Hints.GREEN_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.GREEN_BITS);

                when Window_Hints.BLUE_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.BLUE_BITS);

                when Window_Hints.ALPHA_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.ALPHA_BITS);

                when Window_Hints.DEPTH_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.DEPTH_BITS);

                when Window_Hints.STENCIL_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.STENCIL_BITS);

                when Window_Hints.ACCUM_RED_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.ACCUM_RED_BITS);

                when Window_Hints.ACCUM_GREEN_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.ACCUM_GREEN_BITS);

                when Window_Hints.ACCUM_BLUE_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.ACCUM_BLUE_BITS);

                when Window_Hints.ACCUM_ALPHA_BITS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.ACCUM_ALPHA_BITS);

                when Window_Hints.AUX_BUFFERS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.AUX_BUFFERS);

                when Window_Hints.STEREO =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.STEREO);

                when Window_Hints.SAMPLES =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.SAMPLES);

                when Window_Hints.SRGB_CAPABLE =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.SRGB_CAPABLE);

                when Window_Hints.REFRESH_RATE =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.REFRESH_RATE);

                when Window_Hints.DOUBLEBUFFER =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.DOUBLEBUFFER);

                when Window_Hints.CLIENT_API =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CLIENT_API);

                when Window_Hints.CONTEXT_VERSION_MAJOR =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CONTEXT_VERSION_MAJOR);

                when Window_Hints.CONTEXT_VERSION_MINOR =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CONTEXT_VERSION_MINOR);

                when Window_Hints.CONTEXT_ROBUSTNESS =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CONTEXT_ROBUSTNESS);

                when Window_Hints.OPENGL_FORWARD_COMPAT =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.OPENGL_FORWARD_COMPAT);

                when Window_Hints.OPENGL_DEBUG_CONTEXT =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.OPENGL_DEBUG_CONTEXT);

                when Window_Hints.OPENGL_PROFILE =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.OPENGL_PROFILE);

                when Window_Hints.CONTEXT_RELEASE_BEHAVIOR =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CONTEXT_RELEASE_BEHAVIOR);

                when Window_Hints.CONTEXT_NO_ERROR =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CONTEXT_NO_ERROR);

                when Window_Hints.CONTEXT_CREATION_API =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.CONTEXT_CREATION_API);

                when Window_Hints.SCALE_TO_MONITOR =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.SCALE_TO_MONITOR);

                when Window_Hints.COCOA_RETINA_FRAMEBUFFER =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.COCOA_RETINA_FRAMEBUFFER);

                when Window_Hints.COCOA_FRAME_NAME =>
                    Api.glfwWindowHintString(
                        window_hint       => Hint,
                        window_hint_value => CStrings.New_String(String(hints.COCOA_FRAME_NAME)));

                when Window_Hints.COCOA_GRAPHICS_SWITCHING =>
                    Api.glfwWindowHint(
                        window_hint       => Hint,
                        window_hint_value => hints.COCOA_GRAPHICS_SWITCHING);

                when Window_Hints.X11_CLASS_NAME =>
                    Api.glfwWindowHintString(
                        window_hint       => Hint,
                        window_hint_value => CStrings.New_String(String(hints.X11_CLASS_NAME)));

                when Window_Hints.X11_INSTANCE_NAME =>
                    Api.glfwWindowHintString(
                        window_hint       => Hint,
                        window_hint_value => CStrings.New_String(String(hints.X11_INSTANCE_NAME)));
            end case;

            Error.Raise_If_Present;

        end loop;

    end Window_Set_Hints;


    ----------------------------------------------------------------------------


    function Window_Create
        (
            width               : in     Window_Dimmension;
            height              : in     Window_Dimmension;
            title               : in     String;
            monitor_handle      : in     Glfw_Monitor := No_Monitor;
            share_window_handle : in     Glfw_Window  := No_Window
        )
        return Glfw_Window
    is
        window_handle : Glfw_Window := No_Window;
    begin

       window_handle := Api.glfwCreateWindow(
           width => width,
           height => height,
           title => CStrings.New_String(title),
           monitor => monitor_handle,
           window => share_window_handle);

       Error.Raise_If_Present;

       return window_handle;
    end Window_Create;


    ----------------------------------------------------------------------------


    function Window_Should_Close
        (
            window_handle : in     Glfw_Window
        )
        return Glfw_Bool
    is
       should_close : Glfw_Bool := FALSE;
    begin

        should_close :=
            Api.glfwWindowShouldClose(
                window => window_handle);

        Error.Raise_If_Present;

        return should_close;
    end Window_Should_Close;


    ----------------------------------------------------------------------------


    procedure Window_Destroy
        (
            window_handle : in     Glfw_Window
        )
    is
    begin

       Api.glfwDestroyWindow(
           window => window_handle);

       Error.Raise_If_Present;
    end Window_Destroy;


    ----------------------------------------------------------------------------


    procedure Get_Required_Instance_Extensions
        (
            extensions : in out     Glfw_String_Vector) is

        extension_count : Interfaces.C.unsigned;
        pp_extensions_addr : System.Address := System.Null_Address;

    begin

        pp_extensions_addr :=
            Api.glfwGetRequiredInstanceExtensions(extension_count);

        if (extension_count > 0) and (pp_extensions_addr /= System.Null_Address) then
            declare

                -- Convert System Address to char**
                type Char_Ptr_Array is Array(1 .. Integer(extension_count)) of
                    aliased Interfaces.C.Strings.Chars_Ptr;

                function To_Char_Ptr_Array is new Ada.Unchecked_Conversion(
                    Source => System.Address, Target => Char_Ptr_Array);

                -- Reinterpret system address as array of char pointers
                pp_extensions : constant Char_Ptr_Array := To_Char_Ptr_Array(pp_extensions_addr);

                extension_name : String(1 .. 256);
            begin
                for index in 1 .. Integer(extension_count) loop
                    extension_name := Interfaces.C.Strings.Value(pp_extensions(index));
                    extensions.Append(Glfw_String(extension_name));
                end loop;
            end;
        end if;

        Error.Raise_If_Present;
    end Get_Required_Instance_Extensions;

end Glfw;
