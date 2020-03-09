// Code generated by counterfeiter. DO NOT EDIT.
package workerfakes

import (
	"context"
	"io"
	"sync"
	"time"

	"code.cloudfoundry.org/lager"
	"github.com/concourse/concourse/atc"
	"github.com/concourse/concourse/atc/db"
	"github.com/concourse/concourse/atc/db/lock"
	"github.com/concourse/concourse/atc/resource"
	"github.com/concourse/concourse/atc/runtime"
	"github.com/concourse/concourse/atc/worker"
)

type FakeClient struct {
	CreateVolumeStub        func(lager.Logger, worker.VolumeSpec, worker.WorkerSpec, db.VolumeType) (worker.Volume, error)
	createVolumeMutex       sync.RWMutex
	createVolumeArgsForCall []struct {
		arg1 lager.Logger
		arg2 worker.VolumeSpec
		arg3 worker.WorkerSpec
		arg4 db.VolumeType
	}
	createVolumeReturns struct {
		result1 worker.Volume
		result2 error
	}
	createVolumeReturnsOnCall map[int]struct {
		result1 worker.Volume
		result2 error
	}
	FindContainerStub        func(lager.Logger, int, string) (worker.Container, bool, error)
	findContainerMutex       sync.RWMutex
	findContainerArgsForCall []struct {
		arg1 lager.Logger
		arg2 int
		arg3 string
	}
	findContainerReturns struct {
		result1 worker.Container
		result2 bool
		result3 error
	}
	findContainerReturnsOnCall map[int]struct {
		result1 worker.Container
		result2 bool
		result3 error
	}
	FindVolumeStub        func(lager.Logger, int, string) (worker.Volume, bool, error)
	findVolumeMutex       sync.RWMutex
	findVolumeArgsForCall []struct {
		arg1 lager.Logger
		arg2 int
		arg3 string
	}
	findVolumeReturns struct {
		result1 worker.Volume
		result2 bool
		result3 error
	}
	findVolumeReturnsOnCall map[int]struct {
		result1 worker.Volume
		result2 bool
		result3 error
	}
	RunCheckStepStub        func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, atc.VersionedResourceTypes, time.Duration, resource.Resource) (worker.CheckResult, error)
	runCheckStepMutex       sync.RWMutex
	runCheckStepArgsForCall []struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  atc.VersionedResourceTypes
		arg9  time.Duration
		arg10 resource.Resource
	}
	runCheckStepReturns struct {
		result1 worker.CheckResult
		result2 error
	}
	runCheckStepReturnsOnCall map[int]struct {
		result1 worker.CheckResult
		result2 error
	}
	RunGetStepStub        func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, db.UsedResourceCache, resource.Resource) (worker.GetResult, error)
	runGetStepMutex       sync.RWMutex
	runGetStepArgsForCall []struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  worker.ImageFetcherSpec
		arg9  runtime.ProcessSpec
		arg10 runtime.StartingEventDelegate
		arg11 db.UsedResourceCache
		arg12 resource.Resource
	}
	runGetStepReturns struct {
		result1 worker.GetResult
		result2 error
	}
	runGetStepReturnsOnCall map[int]struct {
		result1 worker.GetResult
		result2 error
	}
	RunPutStepStub        func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, resource.Resource) (worker.PutResult, error)
	runPutStepMutex       sync.RWMutex
	runPutStepArgsForCall []struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  worker.ImageFetcherSpec
		arg9  runtime.ProcessSpec
		arg10 runtime.StartingEventDelegate
		arg11 resource.Resource
	}
	runPutStepReturns struct {
		result1 worker.PutResult
		result2 error
	}
	runPutStepReturnsOnCall map[int]struct {
		result1 worker.PutResult
		result2 error
	}
	RunTaskStepStub        func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, lock.LockFactory) (worker.TaskResult, error)
	runTaskStepMutex       sync.RWMutex
	runTaskStepArgsForCall []struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  worker.ImageFetcherSpec
		arg9  runtime.ProcessSpec
		arg10 runtime.StartingEventDelegate
		arg11 lock.LockFactory
	}
	runTaskStepReturns struct {
		result1 worker.TaskResult
		result2 error
	}
	runTaskStepReturnsOnCall map[int]struct {
		result1 worker.TaskResult
		result2 error
	}
	StreamFileFromArtifactStub        func(context.Context, lager.Logger, runtime.Artifact, string) (io.ReadCloser, error)
	streamFileFromArtifactMutex       sync.RWMutex
	streamFileFromArtifactArgsForCall []struct {
		arg1 context.Context
		arg2 lager.Logger
		arg3 runtime.Artifact
		arg4 string
	}
	streamFileFromArtifactReturns struct {
		result1 io.ReadCloser
		result2 error
	}
	streamFileFromArtifactReturnsOnCall map[int]struct {
		result1 io.ReadCloser
		result2 error
	}
	invocations      map[string][][]interface{}
	invocationsMutex sync.RWMutex
}

func (fake *FakeClient) CreateVolume(arg1 lager.Logger, arg2 worker.VolumeSpec, arg3 worker.WorkerSpec, arg4 db.VolumeType) (worker.Volume, error) {
	fake.createVolumeMutex.Lock()
	ret, specificReturn := fake.createVolumeReturnsOnCall[len(fake.createVolumeArgsForCall)]
	fake.createVolumeArgsForCall = append(fake.createVolumeArgsForCall, struct {
		arg1 lager.Logger
		arg2 worker.VolumeSpec
		arg3 worker.WorkerSpec
		arg4 db.VolumeType
	}{arg1, arg2, arg3, arg4})
	fake.recordInvocation("CreateVolume", []interface{}{arg1, arg2, arg3, arg4})
	fake.createVolumeMutex.Unlock()
	if fake.CreateVolumeStub != nil {
		return fake.CreateVolumeStub(arg1, arg2, arg3, arg4)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	fakeReturns := fake.createVolumeReturns
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeClient) CreateVolumeCallCount() int {
	fake.createVolumeMutex.RLock()
	defer fake.createVolumeMutex.RUnlock()
	return len(fake.createVolumeArgsForCall)
}

func (fake *FakeClient) CreateVolumeCalls(stub func(lager.Logger, worker.VolumeSpec, worker.WorkerSpec, db.VolumeType) (worker.Volume, error)) {
	fake.createVolumeMutex.Lock()
	defer fake.createVolumeMutex.Unlock()
	fake.CreateVolumeStub = stub
}

func (fake *FakeClient) CreateVolumeArgsForCall(i int) (lager.Logger, worker.VolumeSpec, worker.WorkerSpec, db.VolumeType) {
	fake.createVolumeMutex.RLock()
	defer fake.createVolumeMutex.RUnlock()
	argsForCall := fake.createVolumeArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3, argsForCall.arg4
}

func (fake *FakeClient) CreateVolumeReturns(result1 worker.Volume, result2 error) {
	fake.createVolumeMutex.Lock()
	defer fake.createVolumeMutex.Unlock()
	fake.CreateVolumeStub = nil
	fake.createVolumeReturns = struct {
		result1 worker.Volume
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) CreateVolumeReturnsOnCall(i int, result1 worker.Volume, result2 error) {
	fake.createVolumeMutex.Lock()
	defer fake.createVolumeMutex.Unlock()
	fake.CreateVolumeStub = nil
	if fake.createVolumeReturnsOnCall == nil {
		fake.createVolumeReturnsOnCall = make(map[int]struct {
			result1 worker.Volume
			result2 error
		})
	}
	fake.createVolumeReturnsOnCall[i] = struct {
		result1 worker.Volume
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) FindContainer(arg1 lager.Logger, arg2 int, arg3 string) (worker.Container, bool, error) {
	fake.findContainerMutex.Lock()
	ret, specificReturn := fake.findContainerReturnsOnCall[len(fake.findContainerArgsForCall)]
	fake.findContainerArgsForCall = append(fake.findContainerArgsForCall, struct {
		arg1 lager.Logger
		arg2 int
		arg3 string
	}{arg1, arg2, arg3})
	fake.recordInvocation("FindContainer", []interface{}{arg1, arg2, arg3})
	fake.findContainerMutex.Unlock()
	if fake.FindContainerStub != nil {
		return fake.FindContainerStub(arg1, arg2, arg3)
	}
	if specificReturn {
		return ret.result1, ret.result2, ret.result3
	}
	fakeReturns := fake.findContainerReturns
	return fakeReturns.result1, fakeReturns.result2, fakeReturns.result3
}

func (fake *FakeClient) FindContainerCallCount() int {
	fake.findContainerMutex.RLock()
	defer fake.findContainerMutex.RUnlock()
	return len(fake.findContainerArgsForCall)
}

func (fake *FakeClient) FindContainerCalls(stub func(lager.Logger, int, string) (worker.Container, bool, error)) {
	fake.findContainerMutex.Lock()
	defer fake.findContainerMutex.Unlock()
	fake.FindContainerStub = stub
}

func (fake *FakeClient) FindContainerArgsForCall(i int) (lager.Logger, int, string) {
	fake.findContainerMutex.RLock()
	defer fake.findContainerMutex.RUnlock()
	argsForCall := fake.findContainerArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3
}

func (fake *FakeClient) FindContainerReturns(result1 worker.Container, result2 bool, result3 error) {
	fake.findContainerMutex.Lock()
	defer fake.findContainerMutex.Unlock()
	fake.FindContainerStub = nil
	fake.findContainerReturns = struct {
		result1 worker.Container
		result2 bool
		result3 error
	}{result1, result2, result3}
}

func (fake *FakeClient) FindContainerReturnsOnCall(i int, result1 worker.Container, result2 bool, result3 error) {
	fake.findContainerMutex.Lock()
	defer fake.findContainerMutex.Unlock()
	fake.FindContainerStub = nil
	if fake.findContainerReturnsOnCall == nil {
		fake.findContainerReturnsOnCall = make(map[int]struct {
			result1 worker.Container
			result2 bool
			result3 error
		})
	}
	fake.findContainerReturnsOnCall[i] = struct {
		result1 worker.Container
		result2 bool
		result3 error
	}{result1, result2, result3}
}

func (fake *FakeClient) FindVolume(arg1 lager.Logger, arg2 int, arg3 string) (worker.Volume, bool, error) {
	fake.findVolumeMutex.Lock()
	ret, specificReturn := fake.findVolumeReturnsOnCall[len(fake.findVolumeArgsForCall)]
	fake.findVolumeArgsForCall = append(fake.findVolumeArgsForCall, struct {
		arg1 lager.Logger
		arg2 int
		arg3 string
	}{arg1, arg2, arg3})
	fake.recordInvocation("FindVolume", []interface{}{arg1, arg2, arg3})
	fake.findVolumeMutex.Unlock()
	if fake.FindVolumeStub != nil {
		return fake.FindVolumeStub(arg1, arg2, arg3)
	}
	if specificReturn {
		return ret.result1, ret.result2, ret.result3
	}
	fakeReturns := fake.findVolumeReturns
	return fakeReturns.result1, fakeReturns.result2, fakeReturns.result3
}

func (fake *FakeClient) FindVolumeCallCount() int {
	fake.findVolumeMutex.RLock()
	defer fake.findVolumeMutex.RUnlock()
	return len(fake.findVolumeArgsForCall)
}

func (fake *FakeClient) FindVolumeCalls(stub func(lager.Logger, int, string) (worker.Volume, bool, error)) {
	fake.findVolumeMutex.Lock()
	defer fake.findVolumeMutex.Unlock()
	fake.FindVolumeStub = stub
}

func (fake *FakeClient) FindVolumeArgsForCall(i int) (lager.Logger, int, string) {
	fake.findVolumeMutex.RLock()
	defer fake.findVolumeMutex.RUnlock()
	argsForCall := fake.findVolumeArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3
}

func (fake *FakeClient) FindVolumeReturns(result1 worker.Volume, result2 bool, result3 error) {
	fake.findVolumeMutex.Lock()
	defer fake.findVolumeMutex.Unlock()
	fake.FindVolumeStub = nil
	fake.findVolumeReturns = struct {
		result1 worker.Volume
		result2 bool
		result3 error
	}{result1, result2, result3}
}

func (fake *FakeClient) FindVolumeReturnsOnCall(i int, result1 worker.Volume, result2 bool, result3 error) {
	fake.findVolumeMutex.Lock()
	defer fake.findVolumeMutex.Unlock()
	fake.FindVolumeStub = nil
	if fake.findVolumeReturnsOnCall == nil {
		fake.findVolumeReturnsOnCall = make(map[int]struct {
			result1 worker.Volume
			result2 bool
			result3 error
		})
	}
	fake.findVolumeReturnsOnCall[i] = struct {
		result1 worker.Volume
		result2 bool
		result3 error
	}{result1, result2, result3}
}

func (fake *FakeClient) RunCheckStep(arg1 context.Context, arg2 lager.Logger, arg3 db.ContainerOwner, arg4 worker.ContainerSpec, arg5 worker.WorkerSpec, arg6 worker.ContainerPlacementStrategy, arg7 db.ContainerMetadata, arg8 atc.VersionedResourceTypes, arg9 time.Duration, arg10 resource.Resource) (worker.CheckResult, error) {
	fake.runCheckStepMutex.Lock()
	ret, specificReturn := fake.runCheckStepReturnsOnCall[len(fake.runCheckStepArgsForCall)]
	fake.runCheckStepArgsForCall = append(fake.runCheckStepArgsForCall, struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  atc.VersionedResourceTypes
		arg9  time.Duration
		arg10 resource.Resource
	}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10})
	fake.recordInvocation("RunCheckStep", []interface{}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10})
	fake.runCheckStepMutex.Unlock()
	if fake.RunCheckStepStub != nil {
		return fake.RunCheckStepStub(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	fakeReturns := fake.runCheckStepReturns
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeClient) RunCheckStepCallCount() int {
	fake.runCheckStepMutex.RLock()
	defer fake.runCheckStepMutex.RUnlock()
	return len(fake.runCheckStepArgsForCall)
}

func (fake *FakeClient) RunCheckStepCalls(stub func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, atc.VersionedResourceTypes, time.Duration, resource.Resource) (worker.CheckResult, error)) {
	fake.runCheckStepMutex.Lock()
	defer fake.runCheckStepMutex.Unlock()
	fake.RunCheckStepStub = stub
}

func (fake *FakeClient) RunCheckStepArgsForCall(i int) (context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, atc.VersionedResourceTypes, time.Duration, resource.Resource) {
	fake.runCheckStepMutex.RLock()
	defer fake.runCheckStepMutex.RUnlock()
	argsForCall := fake.runCheckStepArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3, argsForCall.arg4, argsForCall.arg5, argsForCall.arg6, argsForCall.arg7, argsForCall.arg8, argsForCall.arg9, argsForCall.arg10
}

func (fake *FakeClient) RunCheckStepReturns(result1 worker.CheckResult, result2 error) {
	fake.runCheckStepMutex.Lock()
	defer fake.runCheckStepMutex.Unlock()
	fake.RunCheckStepStub = nil
	fake.runCheckStepReturns = struct {
		result1 worker.CheckResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunCheckStepReturnsOnCall(i int, result1 worker.CheckResult, result2 error) {
	fake.runCheckStepMutex.Lock()
	defer fake.runCheckStepMutex.Unlock()
	fake.RunCheckStepStub = nil
	if fake.runCheckStepReturnsOnCall == nil {
		fake.runCheckStepReturnsOnCall = make(map[int]struct {
			result1 worker.CheckResult
			result2 error
		})
	}
	fake.runCheckStepReturnsOnCall[i] = struct {
		result1 worker.CheckResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunGetStep(arg1 context.Context, arg2 lager.Logger, arg3 db.ContainerOwner, arg4 worker.ContainerSpec, arg5 worker.WorkerSpec, arg6 worker.ContainerPlacementStrategy, arg7 db.ContainerMetadata, arg8 worker.ImageFetcherSpec, arg9 runtime.ProcessSpec, arg10 runtime.StartingEventDelegate, arg11 db.UsedResourceCache, arg12 resource.Resource) (worker.GetResult, error) {
	fake.runGetStepMutex.Lock()
	ret, specificReturn := fake.runGetStepReturnsOnCall[len(fake.runGetStepArgsForCall)]
	fake.runGetStepArgsForCall = append(fake.runGetStepArgsForCall, struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  worker.ImageFetcherSpec
		arg9  runtime.ProcessSpec
		arg10 runtime.StartingEventDelegate
		arg11 db.UsedResourceCache
		arg12 resource.Resource
	}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12})
	fake.recordInvocation("RunGetStep", []interface{}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12})
	fake.runGetStepMutex.Unlock()
	if fake.RunGetStepStub != nil {
		return fake.RunGetStepStub(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	fakeReturns := fake.runGetStepReturns
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeClient) RunGetStepCallCount() int {
	fake.runGetStepMutex.RLock()
	defer fake.runGetStepMutex.RUnlock()
	return len(fake.runGetStepArgsForCall)
}

func (fake *FakeClient) RunGetStepCalls(stub func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, db.UsedResourceCache, resource.Resource) (worker.GetResult, error)) {
	fake.runGetStepMutex.Lock()
	defer fake.runGetStepMutex.Unlock()
	fake.RunGetStepStub = stub
}

func (fake *FakeClient) RunGetStepArgsForCall(i int) (context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, db.UsedResourceCache, resource.Resource) {
	fake.runGetStepMutex.RLock()
	defer fake.runGetStepMutex.RUnlock()
	argsForCall := fake.runGetStepArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3, argsForCall.arg4, argsForCall.arg5, argsForCall.arg6, argsForCall.arg7, argsForCall.arg8, argsForCall.arg9, argsForCall.arg10, argsForCall.arg11, argsForCall.arg12
}

func (fake *FakeClient) RunGetStepReturns(result1 worker.GetResult, result2 error) {
	fake.runGetStepMutex.Lock()
	defer fake.runGetStepMutex.Unlock()
	fake.RunGetStepStub = nil
	fake.runGetStepReturns = struct {
		result1 worker.GetResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunGetStepReturnsOnCall(i int, result1 worker.GetResult, result2 error) {
	fake.runGetStepMutex.Lock()
	defer fake.runGetStepMutex.Unlock()
	fake.RunGetStepStub = nil
	if fake.runGetStepReturnsOnCall == nil {
		fake.runGetStepReturnsOnCall = make(map[int]struct {
			result1 worker.GetResult
			result2 error
		})
	}
	fake.runGetStepReturnsOnCall[i] = struct {
		result1 worker.GetResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunPutStep(arg1 context.Context, arg2 lager.Logger, arg3 db.ContainerOwner, arg4 worker.ContainerSpec, arg5 worker.WorkerSpec, arg6 worker.ContainerPlacementStrategy, arg7 db.ContainerMetadata, arg8 worker.ImageFetcherSpec, arg9 runtime.ProcessSpec, arg10 runtime.StartingEventDelegate, arg11 resource.Resource) (worker.PutResult, error) {
	fake.runPutStepMutex.Lock()
	ret, specificReturn := fake.runPutStepReturnsOnCall[len(fake.runPutStepArgsForCall)]
	fake.runPutStepArgsForCall = append(fake.runPutStepArgsForCall, struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  worker.ImageFetcherSpec
		arg9  runtime.ProcessSpec
		arg10 runtime.StartingEventDelegate
		arg11 resource.Resource
	}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11})
	fake.recordInvocation("RunPutStep", []interface{}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11})
	fake.runPutStepMutex.Unlock()
	if fake.RunPutStepStub != nil {
		return fake.RunPutStepStub(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	fakeReturns := fake.runPutStepReturns
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeClient) RunPutStepCallCount() int {
	fake.runPutStepMutex.RLock()
	defer fake.runPutStepMutex.RUnlock()
	return len(fake.runPutStepArgsForCall)
}

func (fake *FakeClient) RunPutStepCalls(stub func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, resource.Resource) (worker.PutResult, error)) {
	fake.runPutStepMutex.Lock()
	defer fake.runPutStepMutex.Unlock()
	fake.RunPutStepStub = stub
}

func (fake *FakeClient) RunPutStepArgsForCall(i int) (context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, resource.Resource) {
	fake.runPutStepMutex.RLock()
	defer fake.runPutStepMutex.RUnlock()
	argsForCall := fake.runPutStepArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3, argsForCall.arg4, argsForCall.arg5, argsForCall.arg6, argsForCall.arg7, argsForCall.arg8, argsForCall.arg9, argsForCall.arg10, argsForCall.arg11
}

func (fake *FakeClient) RunPutStepReturns(result1 worker.PutResult, result2 error) {
	fake.runPutStepMutex.Lock()
	defer fake.runPutStepMutex.Unlock()
	fake.RunPutStepStub = nil
	fake.runPutStepReturns = struct {
		result1 worker.PutResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunPutStepReturnsOnCall(i int, result1 worker.PutResult, result2 error) {
	fake.runPutStepMutex.Lock()
	defer fake.runPutStepMutex.Unlock()
	fake.RunPutStepStub = nil
	if fake.runPutStepReturnsOnCall == nil {
		fake.runPutStepReturnsOnCall = make(map[int]struct {
			result1 worker.PutResult
			result2 error
		})
	}
	fake.runPutStepReturnsOnCall[i] = struct {
		result1 worker.PutResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunTaskStep(arg1 context.Context, arg2 lager.Logger, arg3 db.ContainerOwner, arg4 worker.ContainerSpec, arg5 worker.WorkerSpec, arg6 worker.ContainerPlacementStrategy, arg7 db.ContainerMetadata, arg8 worker.ImageFetcherSpec, arg9 runtime.ProcessSpec, arg10 runtime.StartingEventDelegate, arg11 lock.LockFactory) (worker.TaskResult, error) {
	fake.runTaskStepMutex.Lock()
	ret, specificReturn := fake.runTaskStepReturnsOnCall[len(fake.runTaskStepArgsForCall)]
	fake.runTaskStepArgsForCall = append(fake.runTaskStepArgsForCall, struct {
		arg1  context.Context
		arg2  lager.Logger
		arg3  db.ContainerOwner
		arg4  worker.ContainerSpec
		arg5  worker.WorkerSpec
		arg6  worker.ContainerPlacementStrategy
		arg7  db.ContainerMetadata
		arg8  worker.ImageFetcherSpec
		arg9  runtime.ProcessSpec
		arg10 runtime.StartingEventDelegate
		arg11 lock.LockFactory
	}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11})
	fake.recordInvocation("RunTaskStep", []interface{}{arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11})
	fake.runTaskStepMutex.Unlock()
	if fake.RunTaskStepStub != nil {
		return fake.RunTaskStepStub(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	fakeReturns := fake.runTaskStepReturns
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeClient) RunTaskStepCallCount() int {
	fake.runTaskStepMutex.RLock()
	defer fake.runTaskStepMutex.RUnlock()
	return len(fake.runTaskStepArgsForCall)
}

func (fake *FakeClient) RunTaskStepCalls(stub func(context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, lock.LockFactory) (worker.TaskResult, error)) {
	fake.runTaskStepMutex.Lock()
	defer fake.runTaskStepMutex.Unlock()
	fake.RunTaskStepStub = stub
}

func (fake *FakeClient) RunTaskStepArgsForCall(i int) (context.Context, lager.Logger, db.ContainerOwner, worker.ContainerSpec, worker.WorkerSpec, worker.ContainerPlacementStrategy, db.ContainerMetadata, worker.ImageFetcherSpec, runtime.ProcessSpec, runtime.StartingEventDelegate, lock.LockFactory) {
	fake.runTaskStepMutex.RLock()
	defer fake.runTaskStepMutex.RUnlock()
	argsForCall := fake.runTaskStepArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3, argsForCall.arg4, argsForCall.arg5, argsForCall.arg6, argsForCall.arg7, argsForCall.arg8, argsForCall.arg9, argsForCall.arg10, argsForCall.arg11
}

func (fake *FakeClient) RunTaskStepReturns(result1 worker.TaskResult, result2 error) {
	fake.runTaskStepMutex.Lock()
	defer fake.runTaskStepMutex.Unlock()
	fake.RunTaskStepStub = nil
	fake.runTaskStepReturns = struct {
		result1 worker.TaskResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) RunTaskStepReturnsOnCall(i int, result1 worker.TaskResult, result2 error) {
	fake.runTaskStepMutex.Lock()
	defer fake.runTaskStepMutex.Unlock()
	fake.RunTaskStepStub = nil
	if fake.runTaskStepReturnsOnCall == nil {
		fake.runTaskStepReturnsOnCall = make(map[int]struct {
			result1 worker.TaskResult
			result2 error
		})
	}
	fake.runTaskStepReturnsOnCall[i] = struct {
		result1 worker.TaskResult
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) StreamFileFromArtifact(arg1 context.Context, arg2 lager.Logger, arg3 runtime.Artifact, arg4 string) (io.ReadCloser, error) {
	fake.streamFileFromArtifactMutex.Lock()
	ret, specificReturn := fake.streamFileFromArtifactReturnsOnCall[len(fake.streamFileFromArtifactArgsForCall)]
	fake.streamFileFromArtifactArgsForCall = append(fake.streamFileFromArtifactArgsForCall, struct {
		arg1 context.Context
		arg2 lager.Logger
		arg3 runtime.Artifact
		arg4 string
	}{arg1, arg2, arg3, arg4})
	fake.recordInvocation("StreamFileFromArtifact", []interface{}{arg1, arg2, arg3, arg4})
	fake.streamFileFromArtifactMutex.Unlock()
	if fake.StreamFileFromArtifactStub != nil {
		return fake.StreamFileFromArtifactStub(arg1, arg2, arg3, arg4)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	fakeReturns := fake.streamFileFromArtifactReturns
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeClient) StreamFileFromArtifactCallCount() int {
	fake.streamFileFromArtifactMutex.RLock()
	defer fake.streamFileFromArtifactMutex.RUnlock()
	return len(fake.streamFileFromArtifactArgsForCall)
}

func (fake *FakeClient) StreamFileFromArtifactCalls(stub func(context.Context, lager.Logger, runtime.Artifact, string) (io.ReadCloser, error)) {
	fake.streamFileFromArtifactMutex.Lock()
	defer fake.streamFileFromArtifactMutex.Unlock()
	fake.StreamFileFromArtifactStub = stub
}

func (fake *FakeClient) StreamFileFromArtifactArgsForCall(i int) (context.Context, lager.Logger, runtime.Artifact, string) {
	fake.streamFileFromArtifactMutex.RLock()
	defer fake.streamFileFromArtifactMutex.RUnlock()
	argsForCall := fake.streamFileFromArtifactArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3, argsForCall.arg4
}

func (fake *FakeClient) StreamFileFromArtifactReturns(result1 io.ReadCloser, result2 error) {
	fake.streamFileFromArtifactMutex.Lock()
	defer fake.streamFileFromArtifactMutex.Unlock()
	fake.StreamFileFromArtifactStub = nil
	fake.streamFileFromArtifactReturns = struct {
		result1 io.ReadCloser
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) StreamFileFromArtifactReturnsOnCall(i int, result1 io.ReadCloser, result2 error) {
	fake.streamFileFromArtifactMutex.Lock()
	defer fake.streamFileFromArtifactMutex.Unlock()
	fake.StreamFileFromArtifactStub = nil
	if fake.streamFileFromArtifactReturnsOnCall == nil {
		fake.streamFileFromArtifactReturnsOnCall = make(map[int]struct {
			result1 io.ReadCloser
			result2 error
		})
	}
	fake.streamFileFromArtifactReturnsOnCall[i] = struct {
		result1 io.ReadCloser
		result2 error
	}{result1, result2}
}

func (fake *FakeClient) Invocations() map[string][][]interface{} {
	fake.invocationsMutex.RLock()
	defer fake.invocationsMutex.RUnlock()
	fake.createVolumeMutex.RLock()
	defer fake.createVolumeMutex.RUnlock()
	fake.findContainerMutex.RLock()
	defer fake.findContainerMutex.RUnlock()
	fake.findVolumeMutex.RLock()
	defer fake.findVolumeMutex.RUnlock()
	fake.runCheckStepMutex.RLock()
	defer fake.runCheckStepMutex.RUnlock()
	fake.runGetStepMutex.RLock()
	defer fake.runGetStepMutex.RUnlock()
	fake.runPutStepMutex.RLock()
	defer fake.runPutStepMutex.RUnlock()
	fake.runTaskStepMutex.RLock()
	defer fake.runTaskStepMutex.RUnlock()
	fake.streamFileFromArtifactMutex.RLock()
	defer fake.streamFileFromArtifactMutex.RUnlock()
	copiedInvocations := map[string][][]interface{}{}
	for key, value := range fake.invocations {
		copiedInvocations[key] = value
	}
	return copiedInvocations
}

func (fake *FakeClient) recordInvocation(key string, args []interface{}) {
	fake.invocationsMutex.Lock()
	defer fake.invocationsMutex.Unlock()
	if fake.invocations == nil {
		fake.invocations = map[string][][]interface{}{}
	}
	if fake.invocations[key] == nil {
		fake.invocations[key] = [][]interface{}{}
	}
	fake.invocations[key] = append(fake.invocations[key], args)
}

var _ worker.Client = new(FakeClient)
