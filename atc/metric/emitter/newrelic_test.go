package emitter_test

import (
	"code.cloudfoundry.org/lager"
	"github.com/concourse/concourse/atc/metric"
	"github.com/concourse/concourse/atc/metric/emitter"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/onsi/gomega/ghttp"
	"net/http"
	"time"
)

var _ = Describe("NewRelicEmitter", func() {

	var (
		server *ghttp.Server
		client *http.Client
	)
	fakeLogger := lager.NewLogger("newrelic")

	fakeEvent := metric.Event{
		Name:  "build started",
		Value: 1,
		State: metric.EventStateOK,
	}

	BeforeEach(func() {
		server = ghttp.NewServer()
		server.AppendHandlers(ghttp.CombineHandlers(ghttp.RespondWith(http.StatusOK, "")))

		client = &http.Client{
			Transport: &http.Transport{},
			Timeout:   time.Minute,
		}
	})

	AfterEach(func() {
		server.Close()
	})

	Describe("Emit()", func() {
		It("adds events to the NewRelicBatch", func() {
			e := emitter.NewRelicEmitter{
				NewRelicBatch: make([]emitter.NewRelicEvent, 0),
				BatchDuration: 10 * time.Second,
				BatchSize:     10,
				LastEmitTime:  time.Now(),
				Url:           server.URL(),
				Client:        client,
			}

			e.Emit(fakeLogger, fakeEvent)
			Expect(e.NewRelicBatch).To(HaveLen(1))
		})
		It("emits events after it hits batch size", func() {
			e := emitter.NewRelicEmitter{
				NewRelicBatch: make([]emitter.NewRelicEvent, 0),
				BatchDuration: 10 * time.Second,
				BatchSize:     2,
				LastEmitTime:  time.Now(),
				Url:           server.URL(),
				Client:        client,
			}
			e.Emit(fakeLogger, fakeEvent)
			Expect(e.NewRelicBatch).To(HaveLen(1))
			e.Emit(fakeLogger, fakeEvent)
			Expect(e.NewRelicBatch).To(HaveLen(0))
			Eventually(server.ReceivedRequests).Should(HaveLen(1))
		})
		It("emits events after it hits batch duration", func() {
			e := emitter.NewRelicEmitter{
				NewRelicBatch: make([]emitter.NewRelicEvent, 0),
				BatchDuration: 1 * time.Millisecond,
				BatchSize:     100,
				LastEmitTime:  time.Now(),
				Url:           server.URL(),
				Client:        client,
			}
			e.Emit(fakeLogger, fakeEvent)
			Expect(e.NewRelicBatch).To(HaveLen(1))

			Eventually(func() []emitter.NewRelicEvent {
				e.Emit(fakeLogger, fakeEvent)
				return e.NewRelicBatch
			}).Should(HaveLen(0))
			Eventually(server.ReceivedRequests).Should(HaveLen(1))
		})
		It("It has identical json output", func() {
			// TODO: working on this test
			e := emitter.NewRelicEmitter{
				NewRelicBatch: make([]emitter.NewRelicEvent, 0),
				BatchDuration: 10 * time.Second,
				BatchSize:     2,
				LastEmitTime:  time.Now(),
				Url:           server.URL(),
				Client:        client,
			}
			e.Emit(fakeLogger, fakeEvent)
			Expect(e.NewRelicBatch).To(HaveLen(1))
			e.Emit(fakeLogger, fakeEvent)
			Expect(e.NewRelicBatch).To(HaveLen(0))
			Expect(server.ReceivedRequests).To(HaveLen(1))
		})
	})
})
