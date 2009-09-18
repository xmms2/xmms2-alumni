SOURCES := \
	Client.cs \
	Message.cs \
	Result.cs \
	Value/Value.cs \
	Value/Error.cs \
	Value/Integer.cs \
	Value/String.cs \
	Value/List.cs \
	Value/Dictionary.cs

all: Xmms.Client.dll

Xmms.Client.dll: $(SOURCES)
	gmcs -t:library -optimize -out:$@ $(SOURCES)

clean:
	rm -f Xmms.Client.dll
