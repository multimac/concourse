package commands

import (
	"fmt"

	"github.com/concourse/concourse/fly/commands/internal/flaghelpers"
	"github.com/concourse/concourse/fly/rc"
)

type UnpauseJobCommand struct {
	Job flaghelpers.JobFlag `short:"j" long:"job" required:"true" value-name:"PIPELINE/JOB" description:"Name of a job to unpause"`
	TeamFlag
}

func (command *UnpauseJobCommand) Execute(args []string) error {
	pipelineName, jobName := command.Job.PipelineName, command.Job.JobName
	target, err := rc.LoadTarget(Fly.Target, Fly.Verbose)
	if err != nil {
		return err
	}

	err = target.Validate()
	if err != nil {
		return err
	}

	team := command.TeamTarget(target)
	found, err := team.UnpauseJob(pipelineName, jobName)
	if err != nil {
		return err
	}

	if !found {
		return fmt.Errorf("%s/%s not found on team %s\n", pipelineName, jobName, team)
	}

	fmt.Printf("unpaused '%s'\n", jobName)

	return nil
}
