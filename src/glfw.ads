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

package Glfw is

    ----------------------------------------------------------------------------
    -- Named numbers
    ----------------------------------------------------------------------------
    DONT_CARE : constant := -1;
    
    ----------------------------------------------------------------------------
    -- Types
    ----------------------------------------------------------------------------   
    -- A Window Integer Attribute
    type Int_Attrib is new Integer range DONT_CARE .. Integer'Last;
     
    -- Return Codes that can be passed back from GLFW operations
    type Enum_Return_Codes is (
    
        -- No error has occurred.
        NO_ERROR,
        
        -- This occurs if a GLFW function was called that must not be called 
        -- unless the library is initialized.
        --
        -- This is an application programmer error.
        NOT_INITIALIZED,
        
        -- This occurs if a GLFW function was called that needs and operates on
        -- the current OpenGL or OpenGL ES context but no context is current on
        -- the calling thread.
        --
        -- This is an application programmer error.
        NO_CURRENT_CONTEXT,
        
        -- This occurs if one of the arguments to a function was an invalid enum
        -- value.
        --
        -- This is an application programmer error.
        INVALID_ENUM,
        
        -- On of the arguments to the function was an invalid value, for example
        -- requesting a non-existent OpenGL or OpenGL ES version like 2.7.
        --
        -- Requesting a valid but unavailable OpenGL or OpenGL ES version will
        -- instead result in a VERSION_UNAVAILABLE error.
        --
        -- This is an application programmer error.
        INVALID_VALUE,
        
        -- A memory allocation failed.
        --
        -- A bug in GLFW or the underlying operating system.
        OUT_OF_MEMORY,
        
        -- GLFW could not find support for the requested API on the system.
        --
        -- The installed graphics driver does not support the requested API, or
        -- does not support it via the chosen context creation backend.
        API_UNAVAILABLE,
        
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
        VERSION_UNAVAILABLE,
        
        -- A platform-specific error occurred that does not match any of the more
        -- specific categories.
        --
        -- A bug or configuration error in GLFW, the underlying operating system
        -- or its drivers, or a lack of required resources.
        PLATFORM_ERROR,
        
        -- If emitted during window creation, the requested pixel format is not
        -- supported. One or more hard constraints did not match any of the
        -- available pixel formats. 
        --
        -- If emitted when querying the clipboard, the contents of the clipboard
        -- could not be converted to the requested format. 
        FORMAT_UNAVAILABLE,
        
        -- A window that does not have an OpenGL or OpenGL ES context was passed
        -- to a function that requires it to have one.
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
        with Static_Predicate => 
            Enum_Platform_Return_Codes in NO_ERROR | PLATFORM_ERROR;
    
    -- Window Boolean Attribute
    type Enum_Bool_Attrib is (
    
    	-- A true boolean value
        BOOL_TRUE,
        
        -- A false boolean value
        BOOL_FALSE
    );
    -- Values to use for Window Bool Attributes
    for Enum_Bool_Attrib use (
        BOOL_TRUE  => 0,
        BOOL_FALSE => 1
    );
    for Enum_Bool_Attrib'Size use Interfaces.C.int'Size;
    
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
        
        -- No reset Notification robustness strategy
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
    
    -- Window Configuration Data
    type Record_Window_Configuration is record
    
        Resizable               : Enum_Bool_Attrib        := BOOL_TRUE;
        Visible                 : Enum_Bool_Attrib        := BOOL_TRUE;
        Decorated               : Enum_Bool_Attrib        := BOOL_TRUE;
        Focused                 : Enum_Bool_Attrib        := BOOL_TRUE;
        Auto_Iconify            : Enum_Bool_Attrib        := BOOL_TRUE;
        Floating                : Enum_Bool_Attrib        := BOOL_FALSE;
        Maximized               : Enum_Bool_Attrib        := BOOL_FALSE;
        Center_Cursor           : Enum_Bool_Attrib        := BOOL_TRUE;
        Transparent_Framebuffer : Enum_Bool_Attrib        := BOOL_FALSE;
        Focus_On_Show           : Enum_Bool_Attrib        := BOOL_TRUE;
        Scale_To_Monitor        : Enum_Bool_Attrib        := BOOL_FALSE;
        Red_Bits                : Int_Attrib              := 8;
        Green_Bits              : Int_Attrib              := 8;
        Blue_Bits               : Int_Attrib              := 8;
        Alpha_Bits              : Int_Attrib              := 8;
        Depth_Bits              : Int_Attrib              := 24;
        Stencil_Bits            : Int_Attrib              := 8;
        Accum_Red_Bits          : Int_Attrib              := 0;
        Accum_Green_Bits        : Int_Attrib              := 0;
        Accum_Blue_Bits         : Int_Attrib              := 0;
        Accum_Alpha_Bits        : Int_Attrib              := 0;
        Aux_Buffers             : Int_Attrib              := 0;
        Samples                 : Int_Attrib              := 0;
        Refresh_Rate            : Int_Attrib              := DONT_CARE;
        Stereo                  : Enum_Bool_Attrib        := BOOL_FALSE;
        Srgb_Capable            : Enum_Bool_Attrib        := BOOL_FALSE;
        Doublebuffer            : Enum_Bool_Attrib        := BOOL_TRUE;
        Client_Api              : Enum_Client_Api         := OPENGL_API;
        Context_Creation_Api    : Enum_Context_Api        := NATIVE_CONTEXT_API;
        Context_Version_Major   : Int_Attrib              := 1;
        Context_Version_Minor   : Int_Attrib              := 0;
        Context_Robustness      : Enum_Context_Robustness := NO_ROBUSTNESS;
        
    end record;
    
    ----------------------------------------------------------------------------
    -- @brief
    -- This operation initializes GLFW, and should be called before most other
    -- GLFW operations.
    --
    -- If this operation fails, it will free any resources prior to returning the
    -- PLATFORM_ERROR value.
    -- 
    -- If this operation succeeds, all subsequent operations return immediately
    -- with NO_ERROR value.
    -- 
    -- @returns Returns one of the values of Init_Return_Codes.
    ----------------------------------------------------------------------------
    function Init return Enum_Platform_Return_Codes;
    
    ----------------------------------------------------------------------------
    -- @brief
    -- This operation shuts down GLFW, and should be called before exiting the
    -- application to free any resources used by GLFW.
    --
    -- It is possible for the PLATFORM_ERROR return code to be returned.
    ----------------------------------------------------------------------------
    function Shutdown return Enum_Platform_Return_Codes;
    
    
end Glfw;
