SOURCES := \
	Client.cs \
	Message.cs \
	Result.cs \
	ValueResult.cs \
	Value/Value.cs \
	Value/Error.cs \
	Value/Integer.cs \
	Value/String.cs \
	Value/Collection.cs \
	Value/Binary.cs \
	Value/List.cs \
	Value/Dictionary.cs \
    MethodGroup.cs \
    Generated.cs

all: Xmms.Client.dll

Xmms.Client.dll: $(SOURCES)
	gmcs -t:library -optimize -checked -out:$@ $(SOURCES)

Generated.cs: code_generator.py ../../../ipc.xml
	python $< > $@

clean:
	rm -f Generated.cs Xmms.Client.dll Xmms.Client.xml
