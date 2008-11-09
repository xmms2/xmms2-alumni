//
//  .NET bindings for the XMMS2 client library
//
//  Copyright (C) 2005 Daniel Svensson, <nano@nittioonio.nu>
//  Copyright (C) 2008 Tilman Sauerbeck, <tilman@xmms.org>
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Xmms.Client {

	public class Playback {
		public Playback(Client c) {
			client = c;
		}

		public Result Stop() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_stop(client.Connection);

			return new Result(resultHandle);
		}

		public Result Tickle() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_tickle(client.Connection);

			return new Result(resultHandle);
		}

		public Result Start() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_start(
					client.Connection
				);

			return new Result(resultHandle);
		}

		public Result Pause() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_pause(
					client.Connection
				);

			return new Result(resultHandle);
		}

		public Result<Value.UInt32> CurrentID() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_current_id(
					client.Connection
				);

			return new Result<Value.UInt32>(resultHandle);
		}

		public Result SeekMs(uint ms) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_seek_ms(
					client.Connection, ms
				);

			return new Result(resultHandle);
		}

		public Result SeekMsRel(uint ms) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_seek_ms_rel(
					client.Connection, ms
				);

			return new Result(resultHandle);
		}

		public Result SeekSamples(uint samples) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_seek_samples(
					client.Connection, samples
				);

			return new Result(resultHandle);
		}

		public Result SeekSamplesRel(uint samples) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_seek_samples_rel(
					client.Connection, samples
				);

			return new Result(resultHandle);
		}

		public Result<Value.UInt32> Playtime() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_playtime(
					client.Connection
				);

			return new Result<Value.UInt32>(resultHandle);
		}

#if false
		public PlaybackStatus Status() {
			return (PlaybackStatus) Helper.Sync<uint>(NativeMethods.xmmsc_playback_status, client);
		}

		public void Status(Callback<PlaybackStatus> cb) {
			Helper.ASync<Xmms.PlaybackStatus>(NativeMethods.xmmsc_playback_status, client, cb);
		}
#endif

		public Result VolumeSet(
			string channelName, uint channelVolume
		) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_volume_set(
					client.Connection,
					channelName, channelVolume
				);

			return new Result(resultHandle);
		}

		public Result<Value.Dictionary<Value.UInt32>> VolumeGet() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_playback_volume_get(
					client.Connection
				);

			return new Result<Value.Dictionary<Value.UInt32>>(
				resultHandle
			);
		}

#if false
		public void BroadcastVolumeChanged(Callback<uint> cb) {
			Helper.ASync<uint>(NativeMethods.xmmsc_broadcast_playback_volume_changed, client, cb);
		}

		public void BroadcastStatus(Callback<PlaybackStatus> cb) {
			Helper.ASync<Xmms.PlaybackStatus>(NativeMethods.xmmsc_broadcast_playback_status, client, cb);
		}
#endif

		public BroadcastResult<Value.UInt32> BroadcastCurrentID() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_broadcast_playback_current_id(
					client.Connection
				);

			return new BroadcastResult<Value.UInt32>(resultHandle);
		}

#if false
		public void SignalPlaytime (Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint>(NativeMethods.xmmsc_signal_playback_playtime, client, cb);
		}
#endif

		private readonly Client client;
	}
}
