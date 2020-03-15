package body Glfw is

    ----------------------------------------------------------------------------
    -- Platform Functions
    ----------------------------------------------------------------------------


    function Platform_Init return Enum_Platform_Return_Codes is 
    begin
       null
    end Platform_Init;
    
    ----------------------------------------------------------------------------
    -- @brief
    -- This operation shuts down GLFW, and should be called before exiting the
    -- application to free any resources used by GLFW.
    --
    -- It is possible for the PLATFORM_ERROR return code to be returned.
    --
    -- @returns One of the values of Init_Return_Codes.
    ----------------------------------------------------------------------------
    function Platform_Shutdown return Enum_Platform_Return_Codes is
    begin
       null
    end Platform_Shutdown;
    
    ----------------------------------------------------------------------------
    -- @brief
    -- Process GLFW events.
    ----------------------------------------------------------------------------
    procedure Platform_Process_Events is 
    begin 
       null
    end Platform_Process_Events;
    
    ----------------------------------------------------------------------------
    -- Window Functions
    ----------------------------------------------------------------------------
    -- @brief
    -- This operation initializes a GLFW window, and should be called prior to
    -- use of any operation that depends on the existence of a window.
    --
    -- For every member of the window configuration structure a call to set the
    -- corresponding GLFW window Hint is made prior to creating the window.
    --
    -- @param[in]     width         The desired width, in screen coordinates, of 
    --                              the window.
    -- @param[in]     height        The desired height, in screen coordinates, 
    --                              of the window.
    -- @param[in]     title         The initial, UTF-8 encoded window title.
    -- @param[in]     monitor       The monitor to use for full screen mode, or 
    --                              NULL for windowed mode.
    -- @param[in]     share         The window whose context to share resources 
    --                              with, or NULL to not share resources.
    -- @param[in]     window_config The desired configuration for the window.
    -- 
    -- @returns One of the values of Window_Init_Return_Codes.
    ----------------------------------------------------------------------------
    function Window_Init
        (
            width         : in     Window_Dimmension;
            height        : in     Window_Dimmension;
            title         : in     String;
            monitor       : in     Monitor_Id := NONE;
            share         : in     Window_Id  := NONE;
            window_config : in     Record_Window_Configuration;
            window        : out    Window_Id
        )
    return Enum_Window_Init_Return_Codes is
    begin
       null;
    end Window_Init;
    
    ----------------------------------------------------------------------------
    -- @brief
    -- This operation determines whether the it has been requested for a window
    -- to close.
    --
    -- @param[in]     window The window that should be closed.
    --
    -- @returns Boolean True if the window should close, otherwise boolean false.
    ----------------------------------------------------------------------------
    function Window_Should_Close
        (
            window : in     Window_Id
        )
    return Boolean is
    begin
       null;
    end;
    
    ----------------------------------------------------------------------------
    -- @brief 
    -- This operation destroys the given window.
    --
    -- @param[in]     window The window that will be destroyed.
    ----------------------------------------------------------------------------
    procedure Window_Destroy
        (
            window : in     Window_Id
        ) is
     begin
        null;
     end;
        
end Glfw;
