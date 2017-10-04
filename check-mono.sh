#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: check-mono.sh <mono-path>" >&2
    exit 1;
fi

mono_path="$1"; shift
preprocessor_flags="\
	-P -I$mono_path \
	-P -I$mono_path/eglib/src \
	-P -I$mono_path/mono \
	-P -I$mono_path/mono/eglib \
	-P -I$mono_path/mono/eglib/src \
	-P -I/usr/local/opt/openssl/include \
	-P -DHAVE_SGEN_GC \
	-P -DSUPPRESSION_DIR= \
	-P -fno-blocks \
	-P -D_XOPEN_SOURCE"

# profile_flags="+RTS -p -hr -RTS"
profile_flags="+RTS -M5500m -RTS"


translation_units="\
	$mono_path/mono/sgen/sgen-alloc.c \
	$mono_path/mono/sgen/sgen-array-list.c \
	$mono_path/mono/sgen/sgen-cardtable.c \
	$mono_path/mono/sgen/sgen-debug.c \
	$mono_path/mono/sgen/sgen-descriptor.c \
	$mono_path/mono/sgen/sgen-fin-weak-hash.c \
	$mono_path/mono/sgen/sgen-gc.c \
	$mono_path/mono/sgen/sgen-gchandles.c \
	$mono_path/mono/sgen/sgen-gray.c \
	$mono_path/mono/sgen/sgen-hash-table.c \
	$mono_path/mono/sgen/sgen-internal.c \
	$mono_path/mono/sgen/sgen-layout-stats.c \
	$mono_path/mono/sgen/sgen-los.c \
	$mono_path/mono/sgen/sgen-marksweep.c \
	$mono_path/mono/sgen/sgen-memory-governor.c \
	$mono_path/mono/sgen/sgen-nursery-allocator.c \
	$mono_path/mono/sgen/sgen-pinning-stats.c \
	$mono_path/mono/sgen/sgen-pinning.c \
	$mono_path/mono/sgen/sgen-pointer-queue.c \
	$mono_path/mono/sgen/sgen-protocol.c \
	$mono_path/mono/sgen/sgen-qsort.c \
	$mono_path/mono/sgen/sgen-simple-nursery.c \
	$mono_path/mono/sgen/sgen-split-nursery.c \
	$mono_path/mono/sgen/sgen-thread-pool.c \
	$mono_path/mono/sgen/sgen-workers.c"

function mtime() {
	stat -f'%c' "$1"
}

function run_ward() {
    #cabal run ward -- "$@"
    stack exec ward -- "$@"
}

# Emit a graph file for each translation unit
echo "Generating call graphs..." >&2
for translation_unit in $translation_units; do
	translation_unit_modified="$(mtime $translation_unit)"
	if [ -f $translation_unit.graph ]; then
		graph_modified="$(mtime $translation_unit.graph)"
	else
		graph_modified=0
	fi
	if [ "$translation_unit_modified" -lt "$graph_modified" ]; then
		echo "Call graph for $translation_unit is up to date" >&2
	else
		echo "Generating call graph for $translation_unit..." >&2
		time run_ward \
			-- \
			cc \
			--mode=graph \
			--config=mono.config \
			$translation_unit \
			$preprocessor_flags \
			$profile_flags \
			> $translation_unit.graph
		echo "Generated $translation_unit.graph" >&2
	fi
done

graphs=""
for translation_unit in $translation_units; do
	graphs="$graphs $translation_unit.graph"
done

# Check all graph files together
echo "Checking call graphs..." >&2
run_ward \
	-- \
	cc \
	--mode=compiler \
	--config=mono.config \
	$graphs \
	$profile_flags

# "$mono_path/mono/metadata/appdomain.c"
# "$mono_path/mono/metadata/assembly.c"
# "$mono_path/mono/metadata/attach.c"
# "$mono_path/mono/metadata/boehm-gc.c"
# "$mono_path/mono/metadata/class-accessors.c"
# "$mono_path/mono/metadata/class.c"
# "$mono_path/mono/metadata/cominterop.c"
# "$mono_path/mono/metadata/console-unix.c"
# "$mono_path/mono/metadata/coree.c"
# "$mono_path/mono/metadata/custom-attrs.c"
# "$mono_path/mono/metadata/debug-helpers.c"
# "$mono_path/mono/metadata/debug-mono-ppdb.c"
# "$mono_path/mono/metadata/debug-mono-symfile.c"
# "$mono_path/mono/metadata/decimal-ms.c"
# "$mono_path/mono/metadata/domain.c"
# "$mono_path/mono/metadata/dynamic-image.c"
# "$mono_path/mono/metadata/dynamic-stream.c"
# "$mono_path/mono/metadata/environment.c"
# "$mono_path/mono/metadata/exception.c"
# "$mono_path/mono/metadata/file-mmap-posix.c"
# "$mono_path/mono/metadata/filewatcher.c"
# "$mono_path/mono/metadata/gc-stats.c"
# "$mono_path/mono/metadata/gc.c"
# "$mono_path/mono/metadata/handle.c"
# "$mono_path/mono/metadata/icall.c"
# "$mono_path/mono/metadata/image.c"
# "$mono_path/mono/metadata/jit-info.c"
# "$mono_path/mono/metadata/loader.c"
# "$mono_path/mono/metadata/lock-tracer.c"
# "$mono_path/mono/metadata/marshal.c"
# "$mono_path/mono/metadata/mempool.c"
# "$mono_path/mono/metadata/metadata-cross-helpers.c"
# "$mono_path/mono/metadata/metadata-verify.c"
# "$mono_path/mono/metadata/metadata.c"
# "$mono_path/mono/metadata/method-builder.c"
# "$mono_path/mono/metadata/monitor.c"
# "$mono_path/mono/metadata/mono-basic-block.c"
# "$mono_path/mono/metadata/mono-config-dirs.c"
# "$mono_path/mono/metadata/mono-config.c"
# "$mono_path/mono/metadata/mono-debug.c"
# "$mono_path/mono/metadata/mono-endian.c"
# "$mono_path/mono/metadata/mono-hash.c"
# "$mono_path/mono/metadata/mono-mlist.c"
# "$mono_path/mono/metadata/mono-perfcounters.c"
# "$mono_path/mono/metadata/mono-route.c"
# "$mono_path/mono/metadata/mono-security.c"
# "$mono_path/mono/metadata/nacl-stub.c"
# "$mono_path/mono/metadata/null-gc.c"
# "$mono_path/mono/metadata/number-ms.c"
# "$mono_path/mono/metadata/object.c"
# "$mono_path/mono/metadata/opcodes.c"
# "$mono_path/mono/metadata/profiler.c"
# "$mono_path/mono/metadata/property-bag.c"
# "$mono_path/mono/metadata/rand.c"
# "$mono_path/mono/metadata/reflection.c"
# "$mono_path/mono/metadata/remoting.c"
# "$mono_path/mono/metadata/runtime.c"
# "$mono_path/mono/metadata/security-core-clr.c"
# "$mono_path/mono/metadata/security-manager.c"
# "$mono_path/mono/metadata/seq-points-data.c"
# "$mono_path/mono/metadata/sgen-bridge.c"
# "$mono_path/mono/metadata/sgen-mono.c"
# "$mono_path/mono/metadata/sgen-new-bridge.c"
# "$mono_path/mono/metadata/sgen-old-bridge.c"
# "$mono_path/mono/metadata/sgen-stw.c"
# "$mono_path/mono/metadata/sgen-tarjan-bridge.c"
# "$mono_path/mono/metadata/sgen-toggleref.c"
# "$mono_path/mono/metadata/sre-encode.c"
# "$mono_path/mono/metadata/sre-save.c"
# "$mono_path/mono/metadata/sre.c"
# "$mono_path/mono/metadata/string-icalls.c"
# "$mono_path/mono/metadata/sysmath.c"
# "$mono_path/mono/metadata/threadpool-io.c"
# "$mono_path/mono/metadata/threadpool-worker-default.c"
# "$mono_path/mono/metadata/threadpool.c"
# "$mono_path/mono/metadata/threads.c"
# "$mono_path/mono/metadata/verify.c"
# "$mono_path/mono/metadata/w32error-unix.c"
# "$mono_path/mono/metadata/w32event-unix.c"
# "$mono_path/mono/metadata/w32file-unix-glob.c"
# "$mono_path/mono/metadata/w32file-unix.c"
# "$mono_path/mono/metadata/w32file.c"
# "$mono_path/mono/metadata/w32handle-namespace.c"
# "$mono_path/mono/metadata/w32handle.c"
# "$mono_path/mono/metadata/w32mutex-unix.c"
# "$mono_path/mono/metadata/w32process-unix-bsd.c"
# "$mono_path/mono/metadata/w32process-unix-default.c"
# "$mono_path/mono/metadata/w32process-unix.c"
# "$mono_path/mono/metadata/w32process.c"
# "$mono_path/mono/metadata/w32semaphore-unix.c"
# "$mono_path/mono/metadata/w32socket-unix.c"
# "$mono_path/mono/metadata/w32socket.c"
# "$mono_path/mono/mini/abcremoval.c"
# "$mono_path/mono/mini/alias-analysis.c"
# "$mono_path/mono/mini/aot-compiler.c"
# "$mono_path/mono/mini/aot-runtime.c"
# "$mono_path/mono/mini/arch-stubs.c"
# "$mono_path/mono/mini/branch-opts.c"
# "$mono_path/mono/mini/cfgdump.c"
# "$mono_path/mono/mini/cfold.c"
# "$mono_path/mono/mini/debug-mini.c"
# "$mono_path/mono/mini/debugger-agent.c"
# "$mono_path/mono/mini/decompose.c"
# "$mono_path/mono/mini/dominators.c"
# "$mono_path/mono/mini/driver.c"
# "$mono_path/mono/mini/dwarfwriter.c"
# "$mono_path/mono/mini/exceptions-amd64.c"
# "$mono_path/mono/mini/genmdesc.c"
# "$mono_path/mono/mini/graph.c"
# "$mono_path/mono/mini/helpers.c"
# "$mono_path/mono/mini/image-writer.c"
# "$mono_path/mono/mini/interp/interp.c"
# "$mono_path/mono/mini/interp/mintops.c"
# "$mono_path/mono/mini/interp/transform.c"
# "$mono_path/mono/mini/jit-icalls.c"
# "$mono_path/mono/mini/linear-scan.c"
# "$mono_path/mono/mini/liveness.c"
# "$mono_path/mono/mini/lldb.c"
# "$mono_path/mono/mini/local-propagation.c"
# "$mono_path/mono/mini/main-sgen.c"
# "$mono_path/mono/mini/main.c"
# "$mono_path/mono/mini/method-to-ir.c"
# "$mono_path/mono/mini/mini-amd64-gsharedvt.c"
# "$mono_path/mono/mini/mini-amd64.c"
# "$mono_path/mono/mini/mini-codegen.c"
# "$mono_path/mono/mini/mini-cross-helpers.c"
# "$mono_path/mono/mini/mini-exceptions.c"
# "$mono_path/mono/mini/mini-gc.c"
# "$mono_path/mono/mini/mini-generic-sharing.c"
# "$mono_path/mono/mini/mini-native-types.c"
# "$mono_path/mono/mini/mini-posix.c"
# "$mono_path/mono/mini/mini-runtime.c"
# "$mono_path/mono/mini/mini-trampolines.c"
# "$mono_path/mono/mini/mini.c"
# "$mono_path/mono/mini/seq-points.c"
# "$mono_path/mono/mini/simd-intrinsics.c"
# "$mono_path/mono/mini/ssa.c"
# "$mono_path/mono/mini/tasklets.c"
# "$mono_path/mono/mini/trace.c"
# "$mono_path/mono/mini/tramp-amd64-gsharedvt.c"
# "$mono_path/mono/mini/tramp-amd64.c"
# "$mono_path/mono/mini/type-checking.c"
# "$mono_path/mono/mini/unwind.c"
# "$mono_path/mono/mini/xdebug.c"
# "$mono_path/mono/profiler/mono-profiler-aot.c"
# "$mono_path/mono/profiler/mono-profiler-iomap.c"
# "$mono_path/mono/profiler/mono-profiler-log.c"
# "$mono_path/mono/profiler/mprof-report.c"
# "$mono_path/mono/utils/atomic.c"
# "$mono_path/mono/utils/bsearch.c"
# "$mono_path/mono/utils/checked-build.c"
# "$mono_path/mono/utils/dlmalloc.c"
# "$mono_path/mono/utils/hazard-pointer.c"
# "$mono_path/mono/utils/json.c"
# "$mono_path/mono/utils/lock-free-alloc.c"
# "$mono_path/mono/utils/lock-free-array-queue.c"
# "$mono_path/mono/utils/lock-free-queue.c"
# "$mono_path/mono/utils/mach-support-amd64.c"
# "$mono_path/mono/utils/mach-support-unknown.c"
# "$mono_path/mono/utils/mach-support.c"
# "$mono_path/mono/utils/memfuncs.c"
# "$mono_path/mono/utils/mono-codeman.c"
# "$mono_path/mono/utils/mono-conc-hashtable.c"
# "$mono_path/mono/utils/mono-context.c"
# "$mono_path/mono/utils/mono-counters.c"
# "$mono_path/mono/utils/mono-dl-posix.c"
# "$mono_path/mono/utils/mono-dl.c"
# "$mono_path/mono/utils/mono-embed.c"
# "$mono_path/mono/utils/mono-error.c"
# "$mono_path/mono/utils/mono-filemap.c"
# "$mono_path/mono/utils/mono-hwcap-cross.c"
# "$mono_path/mono/utils/mono-hwcap.c"
# "$mono_path/mono/utils/mono-internal-hash.c"
# "$mono_path/mono/utils/mono-io-portability.c"
# "$mono_path/mono/utils/mono-linked-list-set.c"
# "$mono_path/mono/utils/mono-log-android.c"
# "$mono_path/mono/utils/mono-log-common.c"
# "$mono_path/mono/utils/mono-log-posix.c"
# "$mono_path/mono/utils/mono-logger.c"
# "$mono_path/mono/utils/mono-math.c"
# "$mono_path/mono/utils/mono-md5.c"
# "$mono_path/mono/utils/mono-mmap.c"
# "$mono_path/mono/utils/mono-networkinterfaces.c"
# "$mono_path/mono/utils/mono-path.c"
# "$mono_path/mono/utils/mono-poll.c"
# "$mono_path/mono/utils/mono-proclib.c"
# "$mono_path/mono/utils/mono-property-hash.c"
# "$mono_path/mono/utils/mono-publib.c"
# "$mono_path/mono/utils/mono-rand.c"
# "$mono_path/mono/utils/mono-sha1.c"
# "$mono_path/mono/utils/mono-stdlib.c"
# "$mono_path/mono/utils/mono-threads-android.c"
# "$mono_path/mono/utils/mono-threads-coop.c"
# "$mono_path/mono/utils/mono-threads-freebsd.c"
# "$mono_path/mono/utils/mono-threads-linux.c"
# "$mono_path/mono/utils/mono-threads-mach-helper.c"
# "$mono_path/mono/utils/mono-threads-mach.c"
# "$mono_path/mono/utils/mono-threads-netbsd.c"
# "$mono_path/mono/utils/mono-threads-openbsd.c"
# "$mono_path/mono/utils/mono-threads-posix-signals.c"
# "$mono_path/mono/utils/mono-threads-posix.c"
# "$mono_path/mono/utils/mono-threads-state-machine.c"
# "$mono_path/mono/utils/mono-threads.c"
# "$mono_path/mono/utils/mono-time.c"
# "$mono_path/mono/utils/mono-tls.c"
# "$mono_path/mono/utils/mono-uri.c"
# "$mono_path/mono/utils/mono-value-hash.c"
# "$mono_path/mono/utils/monobitset.c"
# "$mono_path/mono/utils/networking-fallback.c"
# "$mono_path/mono/utils/networking-missing.c"
# "$mono_path/mono/utils/networking-posix.c"
# "$mono_path/mono/utils/networking.c"
# "$mono_path/mono/utils/os-event-unix.c"
# "$mono_path/mono/utils/parse.c"
# "$mono_path/mono/utils/strenc.c"
