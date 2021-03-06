package db_test

import (
	"errors"

	"github.com/concourse/concourse/atc/db"
	"github.com/concourse/concourse/atc/db/dbfakes"
	"github.com/lib/pq"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("NotificationBus", func() {

	var (
		c            chan *pq.Notification
		fakeExecutor *dbfakes.FakeExecutor
		fakeListener *dbfakes.FakeListener

		bus db.NotificationsBus
	)

	BeforeEach(func() {
		c = make(chan *pq.Notification, 1)

		fakeExecutor = new(dbfakes.FakeExecutor)
		fakeListener = new(dbfakes.FakeListener)
		fakeListener.NotificationChannelReturns(c)

		bus = db.NewNotificationsBus(fakeListener, fakeExecutor)
	})

	Context("Notify", func() {
		var (
			err error
		)

		JustBeforeEach(func() {
			err = bus.Notify("some-channel")
		})

		It("notifies the channel", func() {
			Expect(fakeExecutor.ExecCallCount()).To(Equal(1))
			msg, _ := fakeExecutor.ExecArgsForCall(0)
			Expect(msg).To(Equal("NOTIFY some-channel"))
		})

		Context("when the executor errors", func() {
			BeforeEach(func() {
				fakeExecutor.ExecReturns(nil, errors.New("nope"))
			})

			It("errors", func() {
				Expect(err).To(HaveOccurred())
			})
		})

		Context("when the executor succeeds", func() {
			BeforeEach(func() {
				fakeExecutor.ExecReturns(nil, nil)
			})

			It("succeeds", func() {
				Expect(err).NotTo(HaveOccurred())
			})
		})
	})

	Context("Listen", func() {
		var (
			err error
		)

		JustBeforeEach(func() {
			_, err = bus.Listen("some-channel")
		})

		Context("when not already listening on channel", func() {
			It("listens on the given channel", func() {
				Expect(fakeListener.ListenCallCount()).To(Equal(1))
				channel := fakeListener.ListenArgsForCall(0)
				Expect(channel).To(Equal("some-channel"))
			})

			Context("when listening errors", func() {
				BeforeEach(func() {
					fakeListener.ListenReturns(errors.New("nope"))
				})

				It("errors", func() {
					Expect(err).To(HaveOccurred())
				})
			})

			Context("when listening succeeds", func() {
				BeforeEach(func() {
					fakeListener.ListenReturns(nil)
				})

				It("succeeds", func() {
					Expect(err).NotTo(HaveOccurred())
				})
			})
		})

		Context("when already listening on the channel", func() {
			BeforeEach(func() {
				_, err := bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())
			})

			It("only listens once", func() {
				Expect(fakeListener.ListenCallCount()).To(Equal(1))
			})
		})
	})

	Context("Unlisten", func() {
		var (
			err error
			c   chan bool
		)

		JustBeforeEach(func() {
			err = bus.Unlisten("some-channel", c)
		})

		Context("when there's only one listener", func() {
			BeforeEach(func() {
				c, err = bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())
			})

			It("unlistens on the given channel", func() {
				Expect(fakeListener.UnlistenCallCount()).To(Equal(1))
				channel := fakeListener.UnlistenArgsForCall(0)
				Expect(channel).To(Equal("some-channel"))
			})

			Context("when unlistening errors", func() {
				BeforeEach(func() {
					fakeListener.UnlistenReturns(errors.New("nope"))
				})

				It("errors", func() {
					Expect(err).To(HaveOccurred())
				})
			})

			Context("when unlistening succeeds", func() {
				BeforeEach(func() {
					fakeListener.UnlistenReturns(nil)
				})

				It("succeeds", func() {
					Expect(err).NotTo(HaveOccurred())
				})
			})
		})

		Context("when there's multiple listeners", func() {
			BeforeEach(func() {
				c, err = bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())

				_, err = bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())
			})

			It("succeeds", func() {
				Expect(err).NotTo(HaveOccurred())
			})

			It("does not unlisten on the given channel", func() {
				Expect(fakeListener.UnlistenCallCount()).To(Equal(0))
			})
		})
	})

	Describe("Receiving Notifications", func() {
		var (
			err error
			a   chan bool
			b   chan bool
		)

		Context("when there are multiple listeners for the same channel", func() {
			BeforeEach(func() {
				a, err = bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())

				b, err = bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())
			})

			Context("when it receives an upstream notification", func() {

				BeforeEach(func() {
					c <- &pq.Notification{Channel: "some-channel"}
				})

				It("delivers the notification to all listeners", func() {
					Eventually(a).Should(Receive(Equal(true)))
					Eventually(b).Should(Receive(Equal(true)))
				})
			})

			Context("when it receives an upstream disconnect notice", func() {

				BeforeEach(func() {
					c <- nil
				})

				It("delivers the notification to all listeners", func() {
					Eventually(a).Should(Receive(Equal(false)))
					Eventually(b).Should(Receive(Equal(false)))
				})
			})

			Context("when one of the listeners unlistens", func() {
				BeforeEach(func() {
					bus.Unlisten("some-channel", a)
				})

				It("should still send notifications to the other listeners", func() {
					c <- &pq.Notification{Channel: "some-channel"}
					Eventually(b).Should(Receive(Equal(true)))
				})
			})
		})

		Context("when there are multiple listeners on different channels", func() {
			BeforeEach(func() {
				a, err = bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())

				b, err = bus.Listen("some-other-channel")
				Expect(err).NotTo(HaveOccurred())
			})

			Context("when it receives an upstream notification", func() {

				BeforeEach(func() {
					c <- &pq.Notification{Channel: "some-channel"}
				})

				It("delivers the notification to only specific listeners", func() {
					Eventually(a).Should(Receive(Equal(true)))
					Consistently(b).ShouldNot(Receive())
				})
			})

			Context("when it receives an upstream disconnect notice", func() {

				BeforeEach(func() {
					c <- nil
				})

				It("delivers the notification to all listeners", func() {
					Eventually(a).Should(Receive(Equal(false)))
					Eventually(b).Should(Receive(Equal(false)))
				})
			})
		})

		Context("when the notification channel fills up while listening", func() {

			BeforeEach(func() {
				fakeListener.ListenCalls(func(_ string) error {
					c <- &pq.Notification{Channel: "some-channel"}
					c <- &pq.Notification{Channel: "some-channel"}
					c <- &pq.Notification{Channel: "some-channel"}
					return nil
				})
			})

			It("should still be able to listen for notifications", func(done Done) {

				_, err := bus.Listen("some-channel")
				Expect(err).NotTo(HaveOccurred())

				_, err = bus.Listen("some-other-channel")
				Expect(err).NotTo(HaveOccurred())

				_, err = bus.Listen("some-new-channel")
				Expect(err).NotTo(HaveOccurred())

				close(done)
			}, 5)

		})

		Context("when the notification channel fills up while unlistening", func() {

			BeforeEach(func() {
				fakeListener.UnlistenCalls(func(_ string) error {
					c <- &pq.Notification{Channel: "some-channel"}
					c <- &pq.Notification{Channel: "some-channel"}
					c <- &pq.Notification{Channel: "some-channel"}
					return nil
				})
			})

			It("should still be able to unlisten for notifications", func(done Done) {

				err := bus.Unlisten("some-channel", a)
				Expect(err).NotTo(HaveOccurred())

				err = bus.Unlisten("some-other-channel", b)
				Expect(err).NotTo(HaveOccurred())

				close(done)
			}, 5)
		})
	})
})
