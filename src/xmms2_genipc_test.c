#include <stdio.h>
#include <xmms2/xmmsclient/xmmsclient.h>

//Compile with
// gcc `pkg-config --cflags --libs xmms2-client` xmms2_genipc_test.c

void callback(void *data);

int main(int argc, char **argv)
{
	xmmsc_connection_t *c = xmmsc_init("CHEESEWHIZ");
	xmmsc_request_t *req;
	xmmsc_request_callback_t cb = callback;
	xmms_ipc_msg_t *msg;

	int i;

	if (!xmmsc_connect(c,NULL))
		return -1;

	if (argc > 1) {
		if(!strncmp(argv[1],"play") || !strncmp(argv[1],"toggleplay")) {
			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_START);
			xmms_ipc_msg_put_uint32 (msg, 1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send (req);
		}
		else if(!strncmp(argv[1],"stop")) {
			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_STOP);
			xmms_ipc_msg_put_uint32 (msg, 1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send (req);
		}
		else if(!strncmp(argv[1],"pause")) {
			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_PAUSE);
			xmms_ipc_msg_put_uint32 (msg, 1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send (req);
		}
		else if(!strncmp(argv[1],"prev")) {
			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_PLAYLIST, XMMSC_PLAYLIST_METHOD_SET_POS_REL);
			xmms_ipc_msg_put_uint32 (msg, 1);
			xmms_ipc_msg_put_int32 (msg, -1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send (req);

			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_KILL);
			xmms_ipc_msg_put_uint32 (msg, 1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send(req);
		}
		else if(!strncmp(argv[1],"next")) {
			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_PLAYLIST, XMMSC_PLAYLIST_METHOD_SET_POS_REL);
			xmms_ipc_msg_put_uint32 (msg, 1);
			xmms_ipc_msg_put_int32 (msg, 1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send (req);

			msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_KILL);
			xmms_ipc_msg_put_uint32 (msg, 1);
			req = xmmsc_request_new (c, msg);
			req->interval = -500;

			xmmsc_request_send(req);
		}
	}

	/*
	//The test clientlib does this for us
	msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_MAIN, XMMSC_MAIN_METHOD_HELLO);
	xmms_ipc_msg_put_uint32 (msg, 1);
	xmms_ipc_msg_put_uint32 (msg, XMMSC_VERSION);
	xmms_ipc_msg_put_string (msg, "GENIPC TEST");
	req = xmmsc_request_new (c, msg);
	//So we don't wait for a reply
	req->interval = -500;

	req->type = 0;

	printf ("My cookie: %d\n",xmms_ipc_msg_get_cookie(req->msg));

	xmmsc_request_send(req);
	*/

//	msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_START);
//	msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_PLAYLIST, XMMSC_PLAYLIST_METHOD_SET_POS_REL);
	//This say we're sending a command and not a property request
//	xmms_ipc_msg_put_uint32 (msg, 1);
	//This say to go back one if the command is SET_POS_REL, otherwise its
	//ignored for METHOD_START
//	xmms_ipc_msg_put_int32 (msg, -1);
//	req = xmmsc_request_new (c, msg);
//	req->interval = -500;
//	req->type = 0;

//	xmmsc_request_send (req);


	/*
	//This will tickle the output if we're changing position
	msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_KILL);
	xmms_ipc_msg_put_uint32 (msg, 1);
	req = xmmsc_request_new (c, msg);
	req->interval = -500;
	req->type = 0;

	xmmsc_request_send(req);
	*/

	msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_OUTPUT, XMMSC_OUTPUT_METHOD_STATUS);
	xmms_ipc_msg_put_uint32 (msg, 1);
	req = xmmsc_request_new (c, msg);
	xmmsc_request_now (req);

	xmmsc_request_set_callback (req, callback);

	xmmsc_request_send (req);

	return 0;
}

// Doesn't do anything yet until I get replies going
void callback(void *data)
{
	int val = *(int *)data;
	printf("STATUS: %d\n", val);
}
