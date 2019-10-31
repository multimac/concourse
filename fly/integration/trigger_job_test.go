package integration_test

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"os/exec"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/vito/go-sse/sse"

	"github.com/concourse/concourse/atc"
	"github.com/concourse/concourse/atc/event"
	"github.com/onsi/gomega/gbytes"
	"github.com/onsi/gomega/gexec"
	"github.com/onsi/gomega/ghttp"
	"github.com/tedsuo/rata"
)

var _ = Describe("Fly CLI", func() {
	Describe("trigger-job", func() {
		var (
			mainPath        string
			otherPath       string
			otherRandomPath string
			err             error
		)

		BeforeEach(func() {
			mainPath, err = atc.Routes.CreatePathForRoute(atc.CreateJobBuild, rata.Params{"pipeline_name": "awesome-pipeline", "job_name": "awesome-job", "team_name": "main"})
			Expect(err).NotTo(HaveOccurred())

			otherPath, err = atc.Routes.CreatePathForRoute(atc.CreateJobBuild, rata.Params{"pipeline_name": "awesome-pipeline", "job_name": "awesome-job", "team_name": "other-team"})
			Expect(err).NotTo(HaveOccurred())

			otherRandomPath, err = atc.Routes.CreatePathForRoute(atc.CreateJobBuild, rata.Params{"pipeline_name": "awesome-pipeline", "job_name": "awesome-job", "team_name": "random-team"})
			Expect(err).NotTo(HaveOccurred())
		})

		Context("when the pipeline and job name are specified", func() {
			Context("when the pipeline and job exists", func() {
				Context("when user owns the same team as the given pipeline", func() {
					Context("user is currently on pipeline's team", func() {

						BeforeEach(func() {
							loginATCServer.AppendHandlers(
								ghttp.CombineHandlers(
									ghttp.VerifyRequest("POST", mainPath),
									ghttp.RespondWithJSONEncoded(http.StatusOK, atc.Build{ID: 57, Name: "42"}),
								),
							)
						})

						It("starts the build", func() {
							Expect(func() {
								flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "awesome-pipeline/awesome-job")

								sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
								Expect(err).NotTo(HaveOccurred())

								Eventually(sess).Should(gbytes.Say(`started awesome-pipeline/awesome-job #42`))

								<-sess.Exited
								Expect(sess.ExitCode()).To(Equal(0))
							}).To(Change(func() int {
								return len(loginATCServer.ReceivedRequests())
							}).By(2))
						})
					})

					Context("user is NOT currently on the pipeline's team", func() {

						BeforeEach(func() {
							loginATCServer.AppendHandlers(
								ghttp.CombineHandlers(
									ghttp.VerifyRequest("POST", otherPath),
									ghttp.RespondWithJSONEncoded(http.StatusOK, atc.Build{ID: 57, Name: "42"}),
								),
							)
						})

						It("starts the build", func() {
							Expect(func() {
								flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "awesome-pipeline/awesome-job", "--team-name", "other-team")

								sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
								Expect(err).NotTo(HaveOccurred())

								Eventually(sess).Should(gbytes.Say(`started awesome-pipeline/awesome-job #42`))

								<-sess.Exited
								Expect(sess.ExitCode()).To(Equal(0))
							}).To(Change(func() int {
								return len(loginATCServer.ReceivedRequests())
							}).By(2))
						})
					})
				})

				Context("when user does NOT own the same team as the given pipeline", func() {
					BeforeEach(func() {
						loginATCServer.AppendHandlers(
							ghttp.CombineHandlers(
								ghttp.VerifyRequest("POST", otherRandomPath),
								ghttp.RespondWith(http.StatusNotFound, nil),
							),
						)
					})

					It("prints an error message", func() {
						Expect(func() {
							flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "awesome-pipeline/awesome-job", "--team-name", "random-team")

							sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
							Expect(err).NotTo(HaveOccurred())

							Eventually(sess.Err).Should(gbytes.Say(`error: resource not found`))

							<-sess.Exited
							Expect(sess.ExitCode()).To(Equal(1))
						}).To(Change(func() int {
							return len(loginATCServer.ReceivedRequests())
						}).By(2))
					})
				})

				Context("when -w option is provided", func() {
					var streaming chan struct{}
					var events chan atc.Event

					BeforeEach(func() {
						streaming = make(chan struct{})
						events = make(chan atc.Event)
						loginATCServer.AppendHandlers(
							ghttp.CombineHandlers(
								ghttp.VerifyRequest("POST", mainPath),
								ghttp.RespondWithJSONEncoded(http.StatusOK, atc.Build{ID: 57, Name: "42"}),
							),
							ghttp.CombineHandlers(
								ghttp.VerifyRequest("GET", "/api/v1/builds/57/events"),
								func(w http.ResponseWriter, r *http.Request) {
									flusher := w.(http.Flusher)

									w.Header().Add("Content-Type", "text/event-stream; charset=utf-8")
									w.Header().Add("Cache-Control", "no-cache, no-store, must-revalidate")
									w.Header().Add("Connection", "keep-alive")

									w.WriteHeader(http.StatusOK)

									flusher.Flush()

									close(streaming)

									id := 0

									for e := range events {
										payload, err := json.Marshal(event.Message{Event: e})
										Expect(err).NotTo(HaveOccurred())

										event := sse.Event{
											ID:   fmt.Sprintf("%d", id),
											Name: "event",
											Data: payload,
										}

										err = event.Write(w)
										Expect(err).NotTo(HaveOccurred())

										flusher.Flush()

										id++
									}

									err := sse.Event{
										Name: "end",
									}.Write(w)
									Expect(err).NotTo(HaveOccurred())
								},
							),
						)
					})

					It("watches the build", func() {
						flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "awesome-pipeline/awesome-job", "-w")

						sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
						Expect(err).NotTo(HaveOccurred())

						Eventually(sess).Should(gbytes.Say(`started awesome-pipeline/awesome-job #42`))
						Eventually(streaming).Should(BeClosed())

						events <- event.Log{Payload: "sup"}

						Eventually(sess.Out).Should(gbytes.Say("sup"))

						close(events)

						<-sess.Exited
						Expect(sess.ExitCode()).To(Equal(0))
					})
				})
			})

			Context("when the pipeline/job doesn't exist", func() {
				BeforeEach(func() {
					loginATCServer.AppendHandlers(
						ghttp.CombineHandlers(
							ghttp.VerifyRequest("POST", otherRandomPath),
							ghttp.RespondWith(http.StatusNotFound, nil),
						),
					)
				})

				It("prints an error message", func() {
					Expect(func() {
						flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "awesome-pipeline/awesome-job", "--team-name", "random-team")

						sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
						Expect(err).NotTo(HaveOccurred())

						Eventually(sess.Err).Should(gbytes.Say(`error: resource not found`))

						<-sess.Exited
						Expect(sess.ExitCode()).To(Equal(1))
					}).To(Change(func() int {
						return len(loginATCServer.ReceivedRequests())
					}).By(2))
				})
			})
		})

		Context("when the pipeline/job name is not specified", func() {
			It("errors", func() {
				reqsBefore := len(loginATCServer.ReceivedRequests())
				flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job")

				sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
				Expect(err).NotTo(HaveOccurred())

				<-sess.Exited
				Expect(sess.ExitCode()).To(Equal(1))
				Expect(loginATCServer.ReceivedRequests()).To(HaveLen(reqsBefore))
			})
		})

		Context("completion", func() {
			BeforeEach(func() {
				os.Setenv("GO_FLAGS_COMPLETION", "1")
			})

			AfterEach(func() {
				os.Unsetenv("GO_FLAGS_COMPLETION")
			})

			It("returns all matching pipelines", func() {
				loginATCServer.AppendHandlers(
					ghttp.CombineHandlers(
						ghttp.VerifyRequest("GET", "/api/v1/teams/main/pipelines"),
						ghttp.RespondWithJSONEncoded(200, []atc.Pipeline{
							{Name: "some-pipeline-1", Paused: false, Public: false},
							{Name: "some-pipeline-2", Paused: false, Public: false},
							{Name: "another-pipeline", Paused: false, Public: false},
						}),
					),
				)

				flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "some-")
				sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
				Expect(err).NotTo(HaveOccurred())
				Eventually(sess).Should(gexec.Exit(0))
				Eventually(sess.Out).Should(gbytes.Say("some-pipeline-1/"))
				Eventually(sess.Out).Should(gbytes.Say("some-pipeline-2/"))
				Eventually(sess.Out).ShouldNot(gbytes.Say("another-pipeline/"))
			})

			It("returns all matching jobs", func() {
				loginATCServer.AppendHandlers(
					ghttp.CombineHandlers(
						ghttp.VerifyRequest("GET", "/api/v1/teams/main/pipelines/some-pipeline/jobs"),
						ghttp.RespondWithJSONEncoded(200, []atc.Job{
							{Name: "some-job-1"},
							{Name: "some-job-2"},
							{Name: "another-job"},
						}),
					),
				)

				flyCmd := exec.Command(flyPath, "-t", "some-target", "trigger-job", "-j", "some-pipeline/some-")
				sess, err := gexec.Start(flyCmd, GinkgoWriter, GinkgoWriter)
				Expect(err).NotTo(HaveOccurred())
				Eventually(sess).Should(gexec.Exit(0))
				Eventually(sess.Out).Should(gbytes.Say("some-pipeline/some-job-1"))
				Eventually(sess.Out).Should(gbytes.Say("some-pipeline/some-job-2"))
				Eventually(sess.Out).ShouldNot(gbytes.Say("some-pipeline/another-job"))
			})
		})
	})
})
