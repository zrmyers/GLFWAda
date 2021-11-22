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
-- @file This package provides a binding to the GLFW C library for Ada. For
--       ease of use, documentation from the glfw3.h header has been worked into
--       this file where necessary.
--------------------------------------------------------------------------------
with Interfaces.C;
with System;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

package Glfw is

    -- The following internal package contains exceptions that can be raised by
    -- GLFW Operations.
    package Exceptions is

        -- This occurs if a GLFW function was called that must not be called
        -- unless the library is initialized.
        --
        -- This is an application programmer error.
        NOT_INITIALIZED : exception;

        -- This occurs if a GLFW function was called that needs and operates on
        -- the current OpenGL or OpenGL ES context but no context is current on
        -- the calling thread.
        --
        -- This is an application programmer error.
        NO_CURRENT_CONTEXT : exception;

        -- This occurs if one of the arguments to a function was an invalid enum
        -- value.
        --
        -- This is an application programmer error.
        INVALID_ENUM : exception;

        -- On of the arguments to the function was an invalid value, for example
        -- requesting a non-existent OpenGL or OpenGL ES version like 2.7.
        --
        -- Requesting a valid but unavailable OpenGL or OpenGL ES version will
        -- instead result in a VERSION_UNAVAILABLE error.
        --
        -- This is an application programmer error.
        INVALID_VALUE : exception;

        -- A memory allocation failed.
        --
        -- A bug in GLFW or the underlying operating system.
        OUT_OF_MEMORY : exception;

        -- GLFW could not find support for the requested API on the system.
        --
        -- The installed graphics driver does not support the requested API, or
        -- does not support it via the chosen context creation backend.
        API_UNAVAILABLE : exception;

        -- The requested OpenGL or OpenGL ES version (including any requested
        -- context or framebuffer hints) is not available on this machine.
        --
        -- The machine does not support the requirements for the application. If
        -- the application is sufficiently flexible it can downgrade its
        -- requirements and try again. Otherwise, the user should be informed that
        -- the machine does not match minimum requirements.
        --
        -- Future invalid OpenGL and OpenGL ES versions, for example OpenGL 4.8
        -- if 5.0 comes out before the 4.x series gets that far, also fail with
        -- this error and not INVALID_VALUE, because GLFW cannot know what future
        -- versions will exist.
        VERSION_UNAVAILABLE : exception;

        -- A platform-specific error occurred that does not match any of the more
        -- specific categories.
        --
        -- A bug or configuration error in GLFW, the underlying operating system
        -- or its drivers, or a lack of required resources.
        PLATFORM_ERROR : exception;

        -- If emitted during window creation, the requested pixel format is not
        -- supported. One or more hard constraints did not match any of the
        -- available pixel formats.
        --
        -- If emitted when querying the clipboard, the contents of the clipboard
        -- could not be converted to the requested format.
        FORMAT_UNAVAILABLE : exception;

        -- A window that does not have an OpenGL or OpenGL ES context was passed
        -- to a function that requires it to have one.
        NO_WINDOW_CONTEXT : exception;
    end Exceptions;

    ----------------------------------------------------------------------------
    -- Constants
    ----------------------------------------------------------------------------
    DONT_CARE         : constant := -1;
    MAX_STRING_LENGTH : constant := 256;
    NONE              : constant := 0;

    ----------------------------------------------------------------------------
    -- Types
    ----------------------------------------------------------------------------
    -- A GLFW Integer Value
    type Glfw_Int is new Integer range DONT_CARE .. Integer'Last;

    --< Common string type used throughout Vulkan interface.
    package Glfw_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => MAX_STRING_LENGTH);

    -- A String Attribute.  Assuming maximum string length of 256
    -- characters
    type Glfw_String is new Glfw_Strings.Bounded_String;

    -- A list of strings.
    package Glfw_String_Vectors is new Ada.Containers.Vectors (
        Index_Type => Natural,
        Element_Type => Glfw_String);

    subtype Glfw_String_Vector is Glfw_String_Vectors.Vector;

    -- Window Boolean Attribute
    type Glfw_Bool is new Boolean;
    -- Values to use for Window Bool Attributes
    for Glfw_Bool use (
        FALSE => 0,
        TRUE  => 1
    );
    for Glfw_Bool'Size use Interfaces.C.int'Size;

    -- An empty Glfw_String.
    GLFW_STRING_EMPTY : constant Glfw_String := To_Bounded_String("");

    -- IDs for GLFW windows
    type Glfw_Window is new System.Address;

    -- A constant which represents the case where no window is passed into or
    -- returned by an operation.
    No_Window : constant Glfw_Window := Glfw_Window(System.Null_Address);

    -- IDs for GLFW monitors
    type Glfw_Monitor is new System.Address;

    -- A constant which represents the case where no monitor is passed into or
    -- returned by an operation.
    No_Monitor : constant Glfw_Monitor := Glfw_Monitor(System.Null_Address);

    -- A Window Dimmension Attribute
    type Window_Dimmension is new Integer range 1 .. Integer'Last;

    -- The client API type
    type Enum_Client_Api is (

        -- No OpenGL API is used with the window.
        NO_API,

        -- The OpenGL API is used with the window.
        OPENGL_API,

        -- The OpenGL ES API is used with the window.
        OPENGL_ES_API
    );
    -- Values to use for the Client API type
    for Enum_Client_Api use (
        NO_API        => 16#00000000#,
        OPENGL_API    => 16#00030001#,
        OPENGL_ES_API => 16#00030002#
    );
    for Enum_Client_Api'Size use Interfaces.C.int'Size;

    -- The context creation API type
    type Enum_Context_Api is (

        -- Native context API is used.
        NATIVE_CONTEXT_API,

        -- EGL context API is used.
        EGL_CONTEXT_API,

        -- OSMESA context API is used.
        OSMESA_CONTEXT_API
    );
    -- Values to use for the Context API type
    for Enum_Context_Api use (
        NATIVE_CONTEXT_API => 16#00036001#,
        EGL_CONTEXT_API    => 16#00036002#,
        OSMESA_CONTEXT_API => 16#00036003#
    );
    for Enum_Context_Api'Size use Interfaces.C.int'Size;

    -- The robustness strategy used by the window's context
    type Enum_Context_Robustness is (

        -- No Context robustness
        NO_ROBUSTNESS,

        -- No reset notification robustness strategy
        NO_RESET_NOTIFICATION,

        -- Lose context on reset robustness strategy
        LOSE_CONTEXT_ON_RESET
    );
    -- Values to use for context robustness.
    for Enum_Context_Robustness use (
        NO_ROBUSTNESS         => 16#00000000#,
        NO_RESET_NOTIFICATION => 16#00031001#,
        LOSE_CONTEXT_ON_RESET => 16#00031002#
    );
    for Enum_Context_Robustness'Size use Interfaces.C.int'Size;

    -- The release behavior used by the context.
    type Enum_Context_Release_Behavior is (
        -- The default behavior of the context creation API is used.
        RELEASE_BEHAVIOR_ANY,

        -- The pipeline will be flushed whenever the context is released from
        -- being the current one.
        RELEASE_BEHAVIOR_FLUSH,

        -- The pipeline will not be flushed on release.
        RELEASE_BEHAVIOR_NONE
    );
    for Enum_Context_Release_Behavior use (
        RELEASE_BEHAVIOR_ANY   => 16#00000000#,
        RELEASE_BEHAVIOR_FLUSH => 16#00035001#,
        RELEASE_BEHAVIOR_NONE  => 16#00035002#
    );
    for Enum_Context_Release_Behavior'Size use Interfaces.C.int'Size;

    -- The OpenGL profile to create the context for.
    type Enum_OpenGl_Profile is (

        -- A specific profile is not requested.
        OPENGL_ANY_PROFILE,

        -- The OpenGL Core profile is requested.
        OPENGL_CORE_PROFILE,

        -- The OpenGL Compatibility profile is requested.
        OPENGL_COMPAT_PROFILE
    );
    for Enum_OpenGl_Profile use (
        OPENGL_ANY_PROFILE    => 16#00000000#,
        OPENGL_CORE_PROFILE   => 16#00032001#,
        OPENGL_COMPAT_PROFILE => 16#00032002#
    );
    for Enum_OpenGl_Profile'Size use Interfaces.C.int'Size;

    -- Window Configuration Data
    type Record_Window_Hints is record

        -- Specifies whether the windowed mode window will be resizable by the
        -- user. The window will be resizable using the Set_Window_Size function.
        -- This hint is ignored for full screen and undecorated windows.
        Resizable                : Glfw_Bool                     := TRUE;

        -- Specifies whether the windowed mode window will initially be visible.
        -- This hint is ignored for full screen windows.
        Visible                  : Glfw_Bool                     := TRUE;

        -- Specifies whether the windowed mode window will have decorations such
        -- as a border, a close widget, etc. An undecorated window will not be
        -- resizable by the user but will still allow the user to generate close
        -- events on some platforms. This hint is ignored for full screen windows.
        Decorated                : Glfw_Bool                     := TRUE;

        -- Specifies whether the windowed mode window will be given input focus
        -- when created. This hint is ignored by full screen and initially hidden
        -- windows.
        Focused                  : Glfw_Bool                     := TRUE;

        -- Specifies whether the full screen window will automatically iconify
        -- and restore the previous video mode on input focus loss. This hint is
        -- ignored for windowed mode windows.
        Auto_Iconify             : Glfw_Bool                     := TRUE;

        -- Specifies whether the windowed mode window will be floating above other
        -- regular windows, also called topmost or always-on-top. This is intended
        -- primarily for debugging purposes and cannot be used to implement proper
        -- full screen windows. This hint is ignored for full screen windows.
        Floating                 : Glfw_Bool                     := FALSE;

        -- Specifies whether the windowed mode window will be maximized when created.
        -- This hint is ignored for full screen windows
        Maximized                : Glfw_Bool                     := FALSE;

        -- Specifies whether the cursor should be centered over newly created full
        -- screen windows. This hint is ignored for windowed mode windows.
        Center_Cursor            : Glfw_Bool                     := TRUE;

        -- Specifies whether the window framebuffer will be transparent. If enabled
        -- and supported by the system, the window framebuffer alpha channel will
        -- be used to combine the framebuffer with the background. This does not
        -- affect window decorations.
        Transparent_Framebuffer  : Glfw_Bool                     := FALSE;

        -- Specifies whether the window will be given input focus when Show_Window
        -- is called.
        Focus_On_Show            : Glfw_Bool                     := TRUE;

        -- Specifies whether the window content area should be resized based on
        -- the monitor content scale of any monitor it is placed on. This includes
        -- the initial placement when the window is created.
        Scale_To_Monitor         : Glfw_Bool                     := FALSE;

        -- The desired bit depth for the Red component of the default framebuffer.
        -- A value of DONT_CARE means the application has no preference.
        Red_Bits                 : Glfw_Int                      := 8;

        -- The desired bit depth for the Green component of the default framebuffer.
        -- A value of DONT_CARE means the application has no preference.
        Green_Bits               : Glfw_Int                      := 8;

        -- The desired bit depth for the Blue component of the default framebuffer.
        -- A value of DONT_CARE means the application has no preference.
        Blue_Bits                : Glfw_Int                      := 8;

        -- The desired bit depth for the Alpha component of the default framebuffer.
        -- A value of DONT_CARE means the application has no preference.
        Alpha_Bits               : Glfw_Int                      := 8;

        -- The desired bit depth for the Depth component of the default framebuffer.
        -- A value of DONT_CARE means the application has no preference.
        Depth_Bits               : Glfw_Int                      := 24;

        -- The desired bit depth for the Stencil component of the default framebuffer.
        -- A value of DONT_CARE means the application has no preference.
        Stencil_Bits             : Glfw_Int                      := 8;

        -- The desired bit depth for the Red component of the accummulation buffer.
        -- A value of DONT_CARE means the application has no preference.
        --
        -- Accumulation buffers are a legacy OpenGL feature and should not be used
        -- in new code.
        Accum_Red_Bits           : Glfw_Int                      := 0;

        -- The desired bit depth for the Green component of the accummulation buffer.
        -- A value of DONT_CARE means the application has no preference.
        --
        -- Accumulation buffers are a legacy OpenGL feature and should not be used
        -- in new code.
        Accum_Green_Bits         : Glfw_Int                      := 0;

        -- The desired bit depth for the Blue component of the accummulation buffer.
        -- A value of DONT_CARE means the application has no preference.
        --
        -- Accumulation buffers are a legacy OpenGL feature and should not be used
        -- in new code.
        Accum_Blue_Bits          : Glfw_Int                      := 0;

        -- The desired bit depth for the Alpha component of the accummulation buffer.
        -- A value of DONT_CARE means the application has no preference.
        --
        -- Accumulation buffers are a legacy OpenGL feature and should not be used
        -- in new code.
        Accum_Alpha_Bits         : Glfw_Int                      := 0;

        -- Specifies the desired number of auxiliary buffers. A value of DONT_CARE
        -- means the application has no preference.
        --
        -- Auxiliary buffers are a legacy OpenGL feature and should not be used
        -- in new code.
        Aux_Buffers              : Glfw_Int                      := 0;

        -- Specifies the desired number of samples to use for multisampling. Zero
        -- disables multisampling. A value of DONT_CARE means the application has
        -- no preference.
        Samples                  : Glfw_Int                      := 0;

        -- Specifies the desired refresh rate for full screen windows. A value of
        -- DONT_CARE means the highest available refresh rate will be used. This
        -- hint is ignored for windowed mode windows.
        Refresh_Rate             : Glfw_Int                      := DONT_CARE;

        -- Specifies whether to use OpenGL stereoscopic rendering. This is a
        -- hard constraint.
        Stereo                   : Glfw_Bool                     := FALSE;

        -- Specifies whether the framebuffer should be sRGB capable.
        --
        --    OpenGL: If enabled and supported by the system, the GL_FRAMEBUFFER_SRGB
        --            enable will control sRGB rendering. By default, sRGB rendering
        --            will be disabled.
        --
        --    OpenGL ES: If enabled and supported by the system, the context will
        --               always have sRGB rendering enabled.
        Srgb_Capable             : Glfw_Bool                     := FALSE;

        -- Specifies whether the framebuffer should be double buffered.  This is a
        -- hard constraint.
        Doublebuffer             : Glfw_Bool                     := TRUE;

        -- Specifies which client API to create the context for. This is a hard
        -- constraint.
        Client_Api               : Enum_Client_Api               := OPENGL_API;

        -- Specifies which context creation API to use to create the context. This
        -- is a hard constraint. If no client API is requested, this hint is ignored.
        Context_Creation_Api     : Enum_Context_Api              := NATIVE_CONTEXT_API;

        -- Specifies the client API major version that the created context must be
        -- compatible with.  The exact behavior of these hints depends on the requested
        -- client API.
        Context_Version_Major    : Glfw_Int                      := 1;

        -- Specifies the client API minor version that the created context must be
        -- compatible with.  The exact behavior of these hints depends on the requested
        -- client API.
        Context_Version_Minor    : Glfw_Int                      := 0;

        -- Specifies the robustness strategy to be used by the context.
        Context_Robustness       : Enum_Context_Robustness       := NO_ROBUSTNESS;

        -- Specifies the release behavior to be used by the context.
        Context_Release_Behavior : Enum_Context_Release_Behavior := RELEASE_BEHAVIOR_ANY;

        -- Specifies whether the context generates errors. If enabled situations
        -- that normally result in errors instead result in undefined behavior.
        Context_No_Error         : Glfw_Bool                     := FALSE;

        -- Specifies whether the OpenGL context should be forward compatible, i.e.
        -- where all functionality deprecated in the requested version of OpenGL is
        -- removed. This must only be used if the requested OpenGL version is 3.0
        -- or above. If OpenGL ES is requested, this hint is ignored.
        OpenGl_Forward_Compat    : Glfw_Bool                     := FALSE;

        -- Specifies whether to use the OpenGL Debug context.
        OpenGl_Debug_Context     : Glfw_Bool                     := FALSE;

        -- Specifies which OpenGL profile to create the context for. If OpenGL ES
        -- is requested, this hint is ignored.
        OpenGl_Profile           : Enum_OpenGl_Profile           := OPENGL_ANY_PROFILE;

        -- Specifies whether to use full resolution framebuffers on Retina displays.
        -- This is only used on macOS platforms.
        Cocoa_Retina_Framebuffer : Glfw_Bool                     := TRUE;

        -- Specifies the UTF-8 encoded name to use for autosaving the window frame,
        -- or if empty disables frame autosaving for the window. This is only used
        -- on macOS platforms.
        Cocoa_Frame_Name         : Glfw_String                   := GLFW_STRING_EMPTY;

        -- Specifies whether to allow the system to choose the integrated GPU for
        -- the OpenGL context and move it between GPUs if necessary or whether to
        -- force it to always run on the discrete GPU. This only affects systems
        -- with both integrated and discrete GPUs. This is only used on macOS
        -- platforms.
        Cocoa_Graphics_Switching : Glfw_Bool                     := FALSE;

        -- Specifies the X11 Class name encoded in ASCII.
        X11_Class_Name           : Glfw_String                   := GLFW_STRING_EMPTY;

        -- Specifes the X11 Instance name encoded in ASCII.
        X11_Instance_Name        : Glfw_String                   := GLFW_STRING_EMPTY;

    end record;


    ----------------------------------------------------------------------------
    -- Platform Operations
    ----------------------------------------------------------------------------
    -- @brief
    -- This operation initializes GLFW, and should be called before most other
    -- GLFW operations.
    --
    -- This operation calls the glfwInit() library function.
    --
    -- When Platform_Init has previously been called without raising an exception,
    -- it will return immediately without raising on subsequent calls.
    --
    -- @error
    -- The following exceptions can be raised by this operation:
    --    PLATFORM_ERROR: When attempting to initialize GLFW on an incompatible
    --                    platform.
    --
    ----------------------------------------------------------------------------
    procedure Platform_Init;


    ----------------------------------------------------------------------------
    -- @brief
    -- This operation shuts down GLFW, and should be called before exiting the
    -- application to free any resources used by GLFW.
    --
    -- This operation calls the glfwTerminate() GLFW library function.
    --
    -- @pre
    -- A call to Platform_Init has occurred without raising an exception.
    --
    -- @error
    -- The following exceptions can be raised by this operation:
    --    PLATFORM_ERROR
    --
    ----------------------------------------------------------------------------
    procedure Platform_Shutdown;


    ----------------------------------------------------------------------------
    -- @brief
    -- Process GLFW events.
    --
    -- This operation calls the glfwPollEvents() GLFW library function.
    --
    -- @pre
    -- A call to Platform_Init has occurred without raising an exception.
    --
    -- @error
    -- The following exceptions can be raised by this operation:
    --    PLATFORM_ERROR
    --
    ----------------------------------------------------------------------------
    procedure Platform_Process_Events;


    ----------------------------------------------------------------------------
    -- Window Operations
    ----------------------------------------------------------------------------
    -- @brief
    -- For every member of the window configuration structure a call to set the
    -- corresponding GLFW window Hint is made.
    --
    -- @pre
    -- A call to Platform_Init has occurred without raising an exception.
    --
    -- @param[in]     window_config The desired configuration for the window.
    --
    -- @error
    -- The following exceptions can be raised by this operation:
    --    PLATFORM_ERROR
    --    NOT_INITIALIZED
    --    INVALID_ENUM
    --
    ----------------------------------------------------------------------------
    procedure Window_Set_Hints
        (
            hints : Record_Window_Hints
        );


    ----------------------------------------------------------------------------
    -- @brief
    -- This operation initializes a GLFW window, and should be called prior to
    -- use of any operation that depends on the existence of a window.
    --
    -- @pre
    -- A call to Platform_Init has occurred without raising an exception.
    --
    -- @param[in]     width               The desired width, in screen coordinates, of
    --                                    the window.
    -- @param[in]     height              The desired height, in screen coordinates,
    --                                    of the window.
    -- @param[in]     title               The initial, UTF-8 encoded window title.
    -- @param[in]     monitor_handle      The monitor to use for full screen mode, or
    --                                    NULL for windowed mode.
    -- @param[in]     share_window_handle The window whose context to share resources
    --                                    with, or NULL to not share resources.
    --
    -- @returns A handle to the created GLFW window.
    --
    -- @error
    -- The following exceptions can be raised by this operation:
    --    PLATFORM_ERROR
    --    NOT_INITIALIZED
    --    INVALID_ENUM
    --    INVALID_VALUE
    --    API_UNAVAILABLE
    --    VERSION_UNAVAILABLE
    --    FORMAT_UNAVAILABLE
    --
    ----------------------------------------------------------------------------
    function Window_Create
        (
            width               : in     Window_Dimmension;
            height              : in     Window_Dimmension;
            title               : in     String;
            monitor_handle      : in     Glfw_Monitor := No_Monitor;
            share_window_handle : in     Glfw_Window  := No_Window
        )
        return Glfw_Window;


    ----------------------------------------------------------------------------
    -- @brief
    -- This operation determines whether the it has been requested for a window
    -- to close.
    --
    -- @pre
    -- A call to Platform_Init() has occurred without raising an exception.
    --
    -- @pre
    -- A call to Window_Create() has occured without raising an exception and
    -- the returned window handle is still valid.
    --
    -- @param[in]     window_handle The window that should be closed.
    --
    -- @returns Boolean True if the window should close, otherwise boolean false.
    --
    -- @errors
    -- The following exceptions can be raised by this operation:
    --     NOT_INITIALIZED
    --
    ----------------------------------------------------------------------------
    function Window_Should_Close
        (
            window_handle : in     Glfw_Window
        )
        return Glfw_Bool;


    ----------------------------------------------------------------------------
    -- @brief
    -- This operation destroys the given window.
    --
    -- @pre
    -- A call to Platform_Init() has occurred without raising an exception.
    --
    -- @pre
    -- A call to Window_Create() has occured without raising an exception and
    -- the returned window handle is still valid.
    --
    -- @param[in]     window_handle The window that will be destroyed.
    --
    -- @errors
    -- The following exceptions can be raised by this operation:
    --     NOT_INITIALIZED
    --     PLATFORM_ERROR
    --
    ----------------------------------------------------------------------------
    procedure Window_Destroy
        (
            window_handle : in     Glfw_Window
        );

    ----------------------------------------------------------------------------
    --< @brief
    --< This operation gets the required instance extensions to use GLFW with
    --< Vulkan.
    --<
    --< @param instance_extensions
    --< A list of extensions required by Vulkan Instance in order to work with GLFW.
    ----------------------------------------------------------------------------
    procedure Get_Required_Instance_Extensions(extensions : in out     Glfw_String_Vector);


end Glfw;
