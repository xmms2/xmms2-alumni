#!perl

use strict;
use warnings;
use Test::More;

eval "use Test::ClassAPI";
plan skip_all => "Test::ClassAPI required" if $@;

use Audio::XMMSClient;
use Audio::XMMSClient::Glib;
use Audio::XMMSClient::Sync;

Test::ClassAPI->execute;

__DATA__

Audio::XMMSClient=class
Audio::XMMSClient::Glib=class
Audio::XMMSClient::Result=class
Audio::XMMSClient::Playlist=class
Audio::XMMSClient::Collection=class
Audio::XMMSClient::Result::PropDict=class
Audio::XMMSClient::Result::PropDict::Tie=class
Audio::XMMSClient::Sync=class
Audio::XMMSClient::Playlist::Sync=class

[Audio::XMMSClient]
new=method
connect=method
disconnect_callback_set=method
io_disconnect=method
get_last_error=method
plugin_list=method
main_stats=method
quit=method
broadcast_quit=method
medialib_get_id=method
medialib_remove_entry=method
medialib_add_entry=method
medialib_add_entry_args=method
medialib_add_entry_encoded=method
playlist=method
medialib_path_import=method
medialib_path_import_encoded=method
medialib_rehash=method
medialib_get_info=method
broadcast_medialib_entry_added=method
broadcast_medialib_entry_changed=method
medialib_entry_property_set_int=method
medialib_entry_property_set_int_with_source=method
medialib_entry_property_set_str=method
medialib_entry_property_set_str_with_source=method
medialib_entry_property_remove=method
medialib_entry_property_remove_with_source=method
coll_get=method
coll_list=method
coll_save=method
coll_remove=method
coll_find=method
coll_rename=method
coll_query_ids=method
coll_query_infos=method
broadcast_collection_changed=method
xform_media_browse=method
xform_media_browse_encoded=method
bindata_add=method
bindata_retrieve=method
bindata_remove=method
configval_register=method
configval_set=method
configval_get=method
configval_list=method
broadcast_configval_changed=method
signal_visualisation_data=method
broadcast_mediainfo_reader_status=method
signal_mediainfo_reader_unindexed=method
userconfdir_get=method
playback_tickle=method
playback_stop=method
playback_pause=method
playback_start=method
playback_seek_ms=method
playback_seek_ms_rel=method
playback_seek_samples=method
playback_seek_samples_rel=method
broadcast_playback_status=method
playback_status=method
broadcast_playback_current_id=method
playback_current_id=method
signal_playback_playtime=method
playback_playtime=method
playback_volume_set=method
playback_volume_get=method
broadcast_playback_volume_changed=method
playlist_list=method
broadcast_playlist_changed=method
broadcast_playlist_current_pos=method
broadcast_playlist_loaded=method
playlist_current_active=method
playlist_set_next=method
playlist_set_next_rel=method
coll_idlist_from_playlist_file=method
io_want_out=method
io_out_handle=method
io_in_handle=method
io_fd_get=method
io_need_out_callback_set=method
loop=method
quit_loop=method
request=method

[Audio::XMMSClient::Glib]
Audio::XMMSClient=isa

[Audio::XMMSClient::Result]
get_class=method
disconnect=method
restart=method
notifier_set=method
wait=method
source_preference_set=method
get_type=method
iserror=method
get_error=method
value=method
decode_url=method

[Audio::XMMSClient::Playlist]
list_entries=method
create=method
current_pos=method
shuffle=method
sort=method
clear=method
insert_id=method
insert_args=method
insert_url=method
insert_encoded=method
insert_collection=method
add_id=method
add_args=method
add_url=method
add_encoded=method
add_collection=method
move_entry=method
remove_entry=method
load=method
radd=method
radd_encoded=method

[Audio::XMMSClient::Collection]
new=method
parse=method
set_idlist=method
add_operand=method
remove_operand=method
idlist_append=method
idlist_insert=method
idlist_move=method
idlist_clear=method
idlist_get_index=method
idlist_set_index=method
idlist_get_size=method
get_type=method
get_idlist=method
operands=method
operand_list_first=method
operand_list_valid=method
operand_list_entry=method
operand_list_next=method
operand_list_save=method
operand_list_restore=method
attribute_set=method
attribute_remove=method
attribute_get=method
attribute_list=method
universe=method

[Audio::XMMSClient::Result::PropDict]
set_source_preference=method
source_hash=method

[Audio::XMMSClient::Result::PropDict::Tie]
FIRSTKEY=method
NEXTKEY=method
FETCH=method

[Audio::XMMSClient::Sync]
new=method
new_from=method
sync_request=method

[Audio::XMMSClient::Playlist::Sync]
Audio::XMMSClient::Sync=isa
