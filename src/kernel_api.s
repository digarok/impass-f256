
;TassFile api.lst - 33321 bytes
;Reading api.lst
kernel_NextEvent = $FF00
kernel_ReadData = $FF04
kernel_ReadExt = $FF08
kernel_Yield = $FF0C
kernel_Putch = $FF10
kernel_RunBlock = $FF14
kernel_RunNamed = $FF18
kernel_BlockDevice_List = $FF20
kernel_BlockDevice_GetName = $FF24
kernel_BlockDevice_GetSize = $FF28
kernel_BlockDevice_Read = $FF2C
kernel_BlockDevice_Write = $FF30
kernel_BlockDevice_Format = $FF34
kernel_BlockDevice_Export = $FF38
kernel_FileSystem_List = $FF3C
kernel_FileSystem_GetSize = $FF40
kernel_FileSystem_MkFS = $FF44
kernel_FileSystem_CheckFS = $FF48
kernel_FileSystem_Mount = $FF4C
kernel_FileSystem_Unmount = $FF50
kernel_FileSystem_ReadBlock = $FF54
kernel_FileSystem_WriteBlock = $FF58
kernel_File_Open = $FF5C
kernel_File_Read = $FF60
kernel_File_Write = $FF64
kernel_File_Close = $FF68
kernel_File_Rename = $FF6C
kernel_File_Delete = $FF70
kernel_File_Seek = $FF74
kernel_Directory_Open = $FF78
kernel_Directory_Read = $FF7C
kernel_Directory_Close = $FF80
kernel_Directory_MkDir = $FF84
kernel_Directory_RmDir = $FF88
kernel_Net_GetIP = $FF90
kernel_Net_SetIP = $FF94
kernel_Net_GetDNS = $FF98
kernel_Net_SetDNS = $FF9C
kernel_Net_SendICMP = $FFA0
kernel_Net_Match = $FFA4
kernel_Net_UDP_Init = $FFA8
kernel_Net_UDP_Send = $FFAC
kernel_Net_UDP_Recv = $FFB0
kernel_Net_TCP_Open = $FFB4
kernel_Net_TCP_Accept = $FFB8
kernel_Net_TCP_Reject = $FFBC
kernel_Net_TCP_Send = $FFC0
kernel_Net_TCP_Recv = $FFC4
kernel_Net_TCP_Close = $FFC8
kernel_Display_Reset = $FFCC
kernel_Display_GetSize = $FFD0
kernel_Display_DrawRow = $FFD4
kernel_Display_DrawColumn = $FFD8
kernel_Clock_GetTime = $FFDC
kernel_Clock_SetTime = $FFE0
kernel_Clock_SetTimer = $FFF0
kernel_args = $F0
kernel_args_events = $F0
kernel_args_events_dest = $F0
kernel_args_events_pending = $F2
kernel_args_events_end = $F3
kernel_args_run = $F3
kernel_args_run_block_id = $F3
kernel_args_recv = $F3
kernel_args_recv_buf = $FB
kernel_args_recv_buflen = $FD
kernel_args_fs = $F3
kernel_args_fs_format = $F3
kernel_args_fs_format_drive = $F3
kernel_args_fs_format_cookie = $F4
kernel_args_fs_format_label = $FB
kernel_args_fs_format_label_len = $FD
kernel_args_fs_mkfs = $F3
kernel_args_fs_mkfs_drive = $F3
kernel_args_fs_mkfs_cookie = $F4
kernel_args_fs_mkfs_label = $FB
kernel_args_fs_mkfs_label_len = $FD
kernel_args_file = $F3
kernel_args_file_open = $F3
kernel_args_file_open_drive = $F3
kernel_args_file_open_cookie = $F4
kernel_args_file_open_fname = $FB
kernel_args_file_open_fname_len = $FD
kernel_args_file_open_mode = $F5
kernel_args_file_open_READ = $0
kernel_args_file_open_WRITE = $1
kernel_args_file_open_END = $2
kernel_args_file_read = $F3
kernel_args_file_read_stream = $F3
kernel_args_file_read_buflen = $F4
kernel_args_file_write = $F3
kernel_args_file_write_stream = $F3
kernel_args_file_write_buf = $FB
kernel_args_file_write_buflen = $FD
kernel_args_file_seek = $F3
kernel_args_file_seek_stream = $F3
kernel_args_file_close = $F3
kernel_args_file_close_stream = $F3
kernel_args_file_rename = $F3
kernel_args_file_rename_drive = $F3
kernel_args_file_rename_cookie = $F4
kernel_args_file_rename_old = $FB
kernel_args_file_rename_old_len = $FD
kernel_args_file_rename_new = $F8
kernel_args_file_rename_new_len = $FA
kernel_args_file_delete = $F3
kernel_args_file_delete_drive = $F3
kernel_args_file_delete_cookie = $F4
kernel_args_file_delete_fname = $FB
kernel_args_file_delete_fname_len = $FD
kernel_args_file_delete_mode = $F5
kernel_args_file_delete_READ = $0
kernel_args_file_delete_WRITE = $1
kernel_args_file_delete_END = $2
kernel_args_directory = $F3
kernel_args_directory_open = $F3
kernel_args_directory_open_drive = $F3
kernel_args_directory_open_cookie = $F4
kernel_args_directory_open_path = $FB
kernel_args_directory_open_path_len = $FD
kernel_args_directory_read = $F3
kernel_args_directory_read_stream = $F3
kernel_args_directory_read_buflen = $F4
kernel_args_directory_close = $F3
kernel_args_directory_close_stream = $F3
kernel_args_directory_mkdir = $F3
kernel_args_directory_mkdir_drive = $F3
kernel_args_directory_mkdir_cookie = $F4
kernel_args_directory_mkdir_path = $FB
kernel_args_directory_mkdir_path_len = $FD
kernel_args_directory_rmdir = $F3
kernel_args_directory_rmdir_drive = $F3
kernel_args_directory_rmdir_cookie = $F4
kernel_args_directory_rmdir_path = $FB
kernel_args_directory_rmdir_path_len = $FD
kernel_args_display = $F3
kernel_args_display_x = $F3
kernel_args_display_y = $F4
kernel_args_display_text = $FB
kernel_args_display_color = $F8
kernel_args_display_buf = $FB
kernel_args_display_buf2 = $F8
kernel_args_display_buflen = $FD
kernel_args_net = $F3
kernel_args_net_socket = $FB
kernel_args_net_src_port = $F3
kernel_args_net_dest_port = $F5
kernel_args_net_dest_ip = $F7
kernel_args_net_accepted = $F3
kernel_args_net_buf = $F8
kernel_args_net_buflen = $FA
kernel_args_config = $F3
kernel_args_timer = $F3
kernel_args_timer_units = $F3
kernel_args_timer_FRAMES = $0
kernel_args_timer_SECONDS = $1
kernel_args_timer_QUERY = $80
kernel_args_timer_absolute = $F4
kernel_args_timer_cookie = $F5
kernel_args_ext = $F8
kernel_args_extlen = $FA
kernel_args_buf = $FB
kernel_args_buflen = $FD
kernel_args_ptr = $FE
kernel_args_t = $0
kernel_args_t_events = $0
kernel_args_t_events_dest = $0
kernel_args_t_events_pending = $2
kernel_args_t_events_end = $3
kernel_args_t_run = $3
kernel_args_t_run_block_id = $3
kernel_args_t_recv = $3
kernel_args_t_recv_buf = $FB
kernel_args_t_recv_buflen = $FD
kernel_args_t_fs = $3
kernel_args_t_fs_format = $3
kernel_args_t_fs_format_drive = $3
kernel_args_t_fs_format_cookie = $4
kernel_args_t_fs_format_label = $FB
kernel_args_t_fs_format_label_len = $FD
kernel_args_t_fs_mkfs = $3
kernel_args_t_fs_mkfs_drive = $3
kernel_args_t_fs_mkfs_cookie = $4
kernel_args_t_fs_mkfs_label = $FB
kernel_args_t_fs_mkfs_label_len = $FD
kernel_args_t_file = $3
kernel_args_t_file_open = $3
kernel_args_t_file_open_drive = $3
kernel_args_t_file_open_cookie = $4
kernel_args_t_file_open_fname = $FB
kernel_args_t_file_open_fname_len = $FD
kernel_args_t_file_open_mode = $5
kernel_args_t_file_open_READ = $0
kernel_args_t_file_open_WRITE = $1
kernel_args_t_file_open_END = $2
kernel_args_t_file_read = $3
kernel_args_t_file_read_stream = $3
kernel_args_t_file_read_buflen = $4
kernel_args_t_file_write = $3
kernel_args_t_file_write_stream = $3
kernel_args_t_file_write_buf = $FB
kernel_args_t_file_write_buflen = $FD
kernel_args_t_file_seek = $3
kernel_args_t_file_seek_stream = $3
kernel_args_t_file_close = $3
kernel_args_t_file_close_stream = $3
kernel_args_t_file_rename = $3
kernel_args_t_file_rename_drive = $3
kernel_args_t_file_rename_cookie = $4
kernel_args_t_file_rename_old = $FB
kernel_args_t_file_rename_old_len = $FD
kernel_args_t_file_rename_new = $F8
kernel_args_t_file_rename_new_len = $FA
kernel_args_t_file_delete = $3
kernel_args_t_file_delete_drive = $3
kernel_args_t_file_delete_cookie = $4
kernel_args_t_file_delete_fname = $FB
kernel_args_t_file_delete_fname_len = $FD
kernel_args_t_file_delete_mode = $5
kernel_args_t_file_delete_READ = $0
kernel_args_t_file_delete_WRITE = $1
kernel_args_t_file_delete_END = $2
kernel_args_t_directory = $3
kernel_args_t_directory_open = $3
kernel_args_t_directory_open_drive = $3
kernel_args_t_directory_open_cookie = $4
kernel_args_t_directory_open_path = $FB
kernel_args_t_directory_open_path_len = $FD
kernel_args_t_directory_read = $3
kernel_args_t_directory_read_stream = $3
kernel_args_t_directory_read_buflen = $4
kernel_args_t_directory_close = $3
kernel_args_t_directory_close_stream = $3
kernel_args_t_directory_mkdir = $3
kernel_args_t_directory_mkdir_drive = $3
kernel_args_t_directory_mkdir_cookie = $4
kernel_args_t_directory_mkdir_path = $FB
kernel_args_t_directory_mkdir_path_len = $FD
kernel_args_t_directory_rmdir = $3
kernel_args_t_directory_rmdir_drive = $3
kernel_args_t_directory_rmdir_cookie = $4
kernel_args_t_directory_rmdir_path = $FB
kernel_args_t_directory_rmdir_path_len = $FD
kernel_args_t_display = $3
kernel_args_t_display_x = $3
kernel_args_t_display_y = $4
kernel_args_t_display_text = $FB
kernel_args_t_display_color = $F8
kernel_args_t_display_buf = $FB
kernel_args_t_display_buf2 = $F8
kernel_args_t_display_buflen = $FD
kernel_args_t_net = $3
kernel_args_t_net_socket = $FB
kernel_args_t_net_src_port = $3
kernel_args_t_net_dest_port = $5
kernel_args_t_net_dest_ip = $7
kernel_args_t_net_accepted = $3
kernel_args_t_net_buf = $F8
kernel_args_t_net_buflen = $FA
kernel_args_t_config = $3
kernel_args_t_timer = $3
kernel_args_t_timer_units = $3
kernel_args_t_timer_FRAMES = $0
kernel_args_t_timer_SECONDS = $1
kernel_args_t_timer_QUERY = $80
kernel_args_t_timer_absolute = $4
kernel_args_t_timer_cookie = $5
kernel_args_t_ext = $F8
kernel_args_t_extlen = $FA
kernel_args_t_buf = $FB
kernel_args_t_buflen = $FD
kernel_args_t_ptr = $FE
kernel_event_t = $0
kernel_event_t_dest = $0
kernel_event_t_pending = $2
kernel_event_t_end = $3
kernel_recv_t = $0
kernel_recv_t_buf = $FB
kernel_recv_t_buflen = $FD
kernel_run_t = $0
kernel_run_t_block_id = $0
kernel_fs_t = $0
kernel_fs_t_format = $0
kernel_fs_t_format_drive = $0
kernel_fs_t_format_cookie = $1
kernel_fs_t_format_label = $FB
kernel_fs_t_format_label_len = $FD
kernel_fs_t_mkfs = $0
kernel_fs_t_mkfs_drive = $0
kernel_fs_t_mkfs_cookie = $1
kernel_fs_t_mkfs_label = $FB
kernel_fs_t_mkfs_label_len = $FD
kernel_fs_mkfs_t = $0
kernel_fs_mkfs_t_drive = $0
kernel_fs_mkfs_t_cookie = $1
kernel_fs_mkfs_t_label = $FB
kernel_fs_mkfs_t_label_len = $FD
kernel_file_t = $0
kernel_file_t_open = $0
kernel_file_t_open_drive = $0
kernel_file_t_open_cookie = $1
kernel_file_t_open_fname = $FB
kernel_file_t_open_fname_len = $FD
kernel_file_t_open_mode = $2
kernel_file_t_open_READ = $0
kernel_file_t_open_WRITE = $1
kernel_file_t_open_END = $2
kernel_file_t_read = $0
kernel_file_t_read_stream = $0
kernel_file_t_read_buflen = $1
kernel_file_t_write = $0
kernel_file_t_write_stream = $0
kernel_file_t_write_buf = $FB
kernel_file_t_write_buflen = $FD
kernel_file_t_seek = $0
kernel_file_t_seek_stream = $0
kernel_file_t_close = $0
kernel_file_t_close_stream = $0
kernel_file_t_rename = $0
kernel_file_t_rename_drive = $0
kernel_file_t_rename_cookie = $1
kernel_file_t_rename_old = $FB
kernel_file_t_rename_old_len = $FD
kernel_file_t_rename_new = $F8
kernel_file_t_rename_new_len = $FA
kernel_file_t_delete = $0
kernel_file_t_delete_drive = $0
kernel_file_t_delete_cookie = $1
kernel_file_t_delete_fname = $FB
kernel_file_t_delete_fname_len = $FD
kernel_file_t_delete_mode = $2
kernel_file_t_delete_READ = $0
kernel_file_t_delete_WRITE = $1
kernel_file_t_delete_END = $2
kernel_fs_open_t = $0
kernel_fs_open_t_drive = $0
kernel_fs_open_t_cookie = $1
kernel_fs_open_t_fname = $FB
kernel_fs_open_t_fname_len = $FD
kernel_fs_open_t_mode = $2
kernel_fs_open_t_READ = $0
kernel_fs_open_t_WRITE = $1
kernel_fs_open_t_END = $2
kernel_fs_read_t = $0
kernel_fs_read_t_stream = $0
kernel_fs_read_t_buflen = $1
kernel_fs_write_t = $0
kernel_fs_write_t_stream = $0
kernel_fs_write_t_buf = $FB
kernel_fs_write_t_buflen = $FD
kernel_fs_seek_t = $0
kernel_fs_seek_t_stream = $0
kernel_fs_close_t = $0
kernel_fs_close_t_stream = $0
kernel_fs_rename_t = $0
kernel_fs_rename_t_drive = $0
kernel_fs_rename_t_cookie = $1
kernel_fs_rename_t_old = $FB
kernel_fs_rename_t_old_len = $FD
kernel_fs_rename_t_new = $F8
kernel_fs_rename_t_new_len = $FA
kernel_fs_delete_t = $0
kernel_fs_delete_t_drive = $0
kernel_fs_delete_t_cookie = $1
kernel_fs_delete_t_fnane = $FB
kernel_fs_delete_t_fname_len = $FD
kernel_dir_t = $0
kernel_dir_t_open = $0
kernel_dir_t_open_drive = $0
kernel_dir_t_open_cookie = $1
kernel_dir_t_open_path = $FB
kernel_dir_t_open_path_len = $FD
kernel_dir_t_read = $0
kernel_dir_t_read_stream = $0
kernel_dir_t_read_buflen = $1
kernel_dir_t_close = $0
kernel_dir_t_close_stream = $0
kernel_dir_t_mkdir = $0
kernel_dir_t_mkdir_drive = $0
kernel_dir_t_mkdir_cookie = $1
kernel_dir_t_mkdir_path = $FB
kernel_dir_t_mkdir_path_len = $FD
kernel_dir_t_rmdir = $0
kernel_dir_t_rmdir_drive = $0
kernel_dir_t_rmdir_cookie = $1
kernel_dir_t_rmdir_path = $FB
kernel_dir_t_rmdir_path_len = $FD
kernel_dir_open_t = $0
kernel_dir_open_t_drive = $0
kernel_dir_open_t_cookie = $1
kernel_dir_open_t_path = $FB
kernel_dir_open_t_path_len = $FD
kernel_dir_read_t = $0
kernel_dir_read_t_stream = $0
kernel_dir_read_t_buflen = $1
kernel_dir_close_t = $0
kernel_dir_close_t_stream = $0
kernel_display_t = $0
kernel_display_t_x = $0
kernel_display_t_y = $1
kernel_display_t_text = $FB
kernel_display_t_color = $F8
kernel_display_t_buf = $FB
kernel_display_t_buf2 = $F8
kernel_display_t_buflen = $FD
kernel_net_t = $0
kernel_net_t_socket = $FB
kernel_net_t_src_port = $0
kernel_net_t_dest_port = $2
kernel_net_t_dest_ip = $4
kernel_net_t_accepted = $0
kernel_net_t_buf = $F8
kernel_net_t_buflen = $FA
kernel_config_t = $0
kernel_timer_t = $0
kernel_timer_t_units = $0
kernel_timer_t_FRAMES = $0
kernel_timer_t_SECONDS = $1
kernel_timer_t_QUERY = $80
kernel_timer_t_absolute = $1
kernel_timer_t_cookie = $2
kernel_time_t = $0
kernel_time_t_century = $0
kernel_time_t_year = $1
kernel_time_t_month = $2
kernel_time_t_day = $3
kernel_time_t_hours = $4
kernel_time_t_minutes = $5
kernel_time_t_seconds = $6
kernel_time_t_centis = $7
kernel_time_t_size = $8
kernel_event_JOYSTICK = $4
kernel_event_DEVICE = $6
kernel_event_key_PRESSED = $8
kernel_event_key_RELEASED = $A
kernel_event_mouse_DELTA = $C
kernel_event_mouse_CLICKS = $E
kernel_event_block_NAME = $10
kernel_event_block_SIZE = $12
kernel_event_block_DATA = $14
kernel_event_block_WROTE = $16
kernel_event_block_FORMATTED = $18
kernel_event_block_ERROR = $1A
kernel_event_fs_SIZE = $1C
kernel_event_fs_CREATED = $1E
kernel_event_fs_CHECKED = $20
kernel_event_fs_DATA = $22
kernel_event_fs_WROTE = $24
kernel_event_fs_ERROR = $26
kernel_event_file_NOT_FOUND = $28
kernel_event_file_OPENED = $2A
kernel_event_file_DATA = $2C
kernel_event_file_WROTE = $2E
kernel_event_file_EOF = $30
kernel_event_file_CLOSED = $32
kernel_event_file_RENAMED = $34
kernel_event_file_DELETED = $36
kernel_event_file_ERROR = $38
kernel_event_file_SEEK = $3A
kernel_event_directory_OPENED = $3C
kernel_event_directory_VOLUME = $3E
kernel_event_directory_FILE = $40
kernel_event_directory_FREE = $42
kernel_event_directory_EOF = $44
kernel_event_directory_CLOSED = $46
kernel_event_directory_ERROR = $48
kernel_event_directory_CREATED = $4A
kernel_event_directory_DELETED = $4C
kernel_event_net_TCP = $4E
kernel_event_net_UDP = $50
kernel_event_timer_EXPIRED = $52
kernel_event_clock_TICK = $54
kernel_event_event_t = $0
kernel_event_event_t_type = $0
kernel_event_event_t_buf = $1
kernel_event_event_t_ext = $2
kernel_event_event_t_key = $3
kernel_event_event_t_key_keyboard = $3
kernel_event_event_t_key_raw = $4
kernel_event_event_t_key_ascii = $5
kernel_event_event_t_key_flags = $6
kernel_event_event_t_key_META = $80
kernel_event_event_t_mouse = $3
kernel_event_event_t_mouse_delta = $3
kernel_event_event_t_mouse_delta_x = $3
kernel_event_event_t_mouse_delta_y = $4
kernel_event_event_t_mouse_delta_z = $5
kernel_event_event_t_mouse_delta_buttons = $6
kernel_event_event_t_mouse_clicks = $3
kernel_event_event_t_mouse_clicks_inner = $3
kernel_event_event_t_mouse_clicks_middle = $4
kernel_event_event_t_mouse_clicks_outer = $5
kernel_event_event_t_joystick = $3
kernel_event_event_t_joystick_joy0 = $3
kernel_event_event_t_joystick_joy1 = $4
kernel_event_event_t_udp = $3
kernel_event_event_t_udp_token = $3
kernel_event_event_t_tcp = $3
kernel_event_event_t_tcp_len = $3
kernel_event_event_t_file = $3
kernel_event_event_t_file_stream = $3
kernel_event_event_t_file_cookie = $4
kernel_event_event_t_file_data = $5
kernel_event_event_t_file_data_requested = $5
kernel_event_event_t_file_data_read = $6
kernel_event_event_t_file_wrote = $5
kernel_event_event_t_file_wrote_requested = $5
kernel_event_event_t_file_wrote_wrote = $6
kernel_event_event_t_directory = $3
kernel_event_event_t_directory_stream = $3
kernel_event_event_t_directory_cookie = $4
kernel_event_event_t_directory_volume = $5
kernel_event_event_t_directory_volume_len = $5
kernel_event_event_t_directory_volume_flags = $6
kernel_event_event_t_directory_file = $5
kernel_event_event_t_directory_file_len = $5
kernel_event_event_t_directory_file_flags = $6
kernel_event_event_t_directory_free = $5
kernel_event_event_t_directory_free_flags = $5
kernel_event_event_t_timer = $3
kernel_event_event_t_timer_value = $3
kernel_event_event_t_timer_cookie = $4
kernel_event_key_t = $0
kernel_event_key_t_keyboard = $0
kernel_event_key_t_raw = $1
kernel_event_key_t_ascii = $2
kernel_event_key_t_flags = $3
kernel_event_key_t_META = $80
kernel_event_mouse_t = $0
kernel_event_mouse_t_delta = $0
kernel_event_mouse_t_delta_x = $0
kernel_event_mouse_t_delta_y = $1
kernel_event_mouse_t_delta_z = $2
kernel_event_mouse_t_delta_buttons = $3
kernel_event_mouse_t_clicks = $0
kernel_event_mouse_t_clicks_inner = $0
kernel_event_mouse_t_clicks_middle = $1
kernel_event_mouse_t_clicks_outer = $2
kernel_event_m_delta_t = $0
kernel_event_m_delta_t_x = $0
kernel_event_m_delta_t_y = $1
kernel_event_m_delta_t_z = $2
kernel_event_m_delta_t_buttons = $3
kernel_event_m_clicks_t = $0
kernel_event_m_clicks_t_inner = $0
kernel_event_m_clicks_t_middle = $1
kernel_event_m_clicks_t_outer = $2
kernel_event_joystick_t = $0
kernel_event_joystick_t_joy0 = $0
kernel_event_joystick_t_joy1 = $1
kernel_event_file_t = $0
kernel_event_file_t_stream = $0
kernel_event_file_t_cookie = $1
kernel_event_file_t_data = $2
kernel_event_file_t_data_requested = $2
kernel_event_file_t_data_read = $3
kernel_event_file_t_wrote = $2
kernel_event_file_t_wrote_requested = $2
kernel_event_file_t_wrote_wrote = $3
kernel_event_fs_data_t = $0
kernel_event_fs_data_t_requested = $0
kernel_event_fs_data_t_read = $1
kernel_event_fs_wrote_t = $0
kernel_event_fs_wrote_t_requested = $0
kernel_event_fs_wrote_t_wrote = $1
kernel_event_dir_t = $0
kernel_event_dir_t_stream = $0
kernel_event_dir_t_cookie = $1
kernel_event_dir_t_volume = $2
kernel_event_dir_t_volume_len = $2
kernel_event_dir_t_volume_flags = $3
kernel_event_dir_t_file = $2
kernel_event_dir_t_file_len = $2
kernel_event_dir_t_file_flags = $3
kernel_event_dir_t_free = $2
kernel_event_dir_t_free_flags = $2
kernel_event_dir_vol_t = $0
kernel_event_dir_vol_t_len = $0
kernel_event_dir_vol_t_flags = $1
kernel_event_dir_file_t = $0
kernel_event_dir_file_t_len = $0
kernel_event_dir_file_t_flags = $1
kernel_event_dir_free_t = $0
kernel_event_dir_free_t_flags = $0
kernel_event_dir_ext_t = $0
kernel_event_dir_ext_t_free = $0
kernel_event_udp_t = $0
kernel_event_udp_t_token = $0
kernel_event_tcp_t = $0
kernel_event_tcp_t_len = $0
kernel_event_timer_t = $0
kernel_event_timer_t_value = $0
kernel_event_timer_t_cookie = $1

;Read Completed
