# Title/Abstract Screening with Local Ollama Models

## Introduction

Screening titles and abstracts through online APIs can raise privacy
concerns and reproducibility challenges. This vignette introduces
[`tabscreen_ollama()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_ollama.md),
a function that uses locally hosted Ollama models screen titles and
abstracts while maintaining data privacy and control. This function
builds on the
[`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
approach, adapting it for local execution with Ollama models. Therefore,
the output structure and key arguments remain the same but data remains
on the local machine. This gives users full control over model versions
and data handling, making it suitable for sensitive or regulated
datasets. Furthermore, this approach allows users to use all Ollama
models including their own custom or finetuned models as long as these
are installed within the ollama framework. Using Ollama locally also
eliminates the need for API keys. The user should however be aware that
Ollama models are typically large and require substantial local compute
resources (e.g., RAM, disk space, and possibly a GPU) to run
efficiently.

### Get started

Before using
[`tabscreen_ollama()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_ollama.md),
make sure Ollama is installed and at least one compatible model is
available locally (or via cloud). For more information on Ollama cloud
see [Ollama Cloud](https://docs.ollama.com/cloud).

#### 1) Install Ollama

Install Ollama from the official website: <https://ollama.com/download>.

#### 2) Check whether Ollama is running and list local models

You can view locally installed models with:

``` bash
ollama ls
```

If this returns an empty list, you need to pull at least one model
before screening.

#### 3) Find a model to use

Browse available models in the official model library:
<https://ollama.com/models>

Choose a model that supports tool/function-calling, as
[`tabscreen_ollama()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_ollama.md)
relies on structured output. A model that supports tool/function-calling
will have a `tools` tag in model documentation. An example model that
supports tools is `llama3.2:latest`.

![](figures%5Collama_llama32.png)

*llama3.2:latest documentation*.

#### 4) Install (pull) a model locally

Pull the selected model in your terminal. For example:

``` bash
ollama pull llama3.2:latest
```

You can then confirm installation again with:

``` bash
ollama ls
```

![](figures%5Collama_installation.png)

*ollama installation and verification of llama3.2:latest*.

Note: An alternative way to install models is through the Ollama desktop
app, which provides an interface for managing models and interacting
with the local Ollama server. However, we recommend using the terminal
for installation to ensure the correct model version is installed and to
checking the model’s tool/function-calling capabilities.

### Data

Data is requiered to be in the same format as for
[`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md),
i.e. a data.frame or tibble with at least three columns: `studyid`,
`title`, and `abstract`. For demonstration purposes, we use a small
example dataset from the `AIscreenR` package.

``` r
# Load example data
data("filges2015_dat", package = "AIscreenR")
filges2015_dat <- filges2015_dat[1:5, ] # Using only first 5 rows for demonstration
head(filges2015_dat)
```

                                                                          author eppi_id studyid
    1                                  Holloway R G and Gramling R and Kelly A G 9434957       1
    2   Morawska Alina and Stallman Helen M and Sanders Matthew R and Ralph Alan 9433838       2
    3 Michel C M and Pascual-Marqui R D and Strik W K and Koenig T and Lehmann D 9431171       3
    4                                                              Paul Howard A 9433968       4
    5                     Feinberg I and De Bie E and Davis N M and Campbell I G 9434460       5
                                                                                                                      title
    1                                                 Estimating and communicating prognosis in advanced neurologic disease
    2                                                   Self-Directed Behavioral Family Intervention: Do Therapists Matter?
    3                         Frequency domain source localization shows state-dependent diazepam effects in 47-channel EEG
    4 A Review of: 'Kearney, C. A. (2010). Helping Children with Selective Mutism: A Guide for School-Based Professionals.'
    5                           Topographic differences in the adolescent maturation of the slow wave EEG during NREM sleep
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         abstract
    1                                                                                                                                                                                                                                                                                                   Prognosis can no longer be relegated behind diagnosis and therapy in high-quality neurologic care. High-stakes decisions that patients (or their surrogates) make often rest upon perceptions and beliefs about prognosis, many of which are poorly informed. The new science of prognostication-the estimating and communication "what to expect"-is in its infancy and the evidence base to support "best practices" is lacking. We propose a framework for formulating a prediction and communicating "what to expect" with patients, families, and surrogates in the context of common neurologic illnesses. Because neurologic disease affects function as much as survival, we specifically address 2 important prognostic questions: "How long?" and "How well?" We provide a summary of prognostic information and highlight key points when tailoring a prognosis for common neurologic diseases. We discuss the challenges of managing prognostic uncertainty, balancing hope and realism, and ways to effectively engage surrogate decision-makers. We also describe what is known about the nocebo effects and the self-fulfilling prophecy when communicating prognoses. There is an urgent need to establish research and educational priorities to build a credible evidence base to support best practices, improve communication skills, and optimize decision-making. Confronting the challenges of prognosis is necessary to fulfill the promise of delivering high-quality, patient-centered care. Neurology (R) 2013;80:764-772 Prognosis can no longer be relegated behind diagnosis and therapy in high-quality neurologic care. High-stakes decisions that patients (or their surrogates) make often rest upon perceptions and beliefs about prognosis, many of which are poorly informed. The new science of prognostication-the estimating and communication "what to expect"-is in its infancy and the evidence base to support "best practices" is lacking. We propose a framework for formulating a prediction and communicating "what to expect" with patients, families, and surrogates in the context of common neurologic illnesses. Because neurologic disease affects function as much as survival, we specifically address 2 important prognostic questions: "How long?" and "How well?" We provide a summary of prognostic information and highlight key points when tailoring a prognosis for common neurologic diseases. We discuss the challenges of managing prognostic uncertainty, balancing hope and realism, and ways to effectively engage surrogate decision-makers. We also describe what is known about the nocebo effects and the self-fulfilling prophecy when communicating prognoses. There is an urgent need to establish research and educational priorities to build a credible evidence base to support best practices, improve communication skills, and optimize decision-making. Confronting the challenges of prognosis is necessary to fulfill the promise of delivering high-quality, patient-centered care. Neurology (R) 2013;80:764-772
    2                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Behavioral family intervention is an effective form of intervention for the prevention and treatment of a wide range of emotional and behavioral problems in children. There is a growing need to address the accessibility of these services. This paper reviews the literature on self-directed interventions designed to help parents manage difficult child behaviors. Evidence regarding the efficacy of interventions is reviewed, and some of the difficulties associated with self-directed programs are discussed. The Self-directed Triple P and Teen Triple P-Positive Parenting Programs are highlighted as examples of efficacious and effective behavioral family interventions fitting into a larger multilevel model of family intervention. The discussion of the efficacy and effectiveness of self-directed Triple P has implications for service delivery of parenting programs. [ABSTRACT FROM AUTHOR] Copyright of Child & Family Behavior Therapy is the property of Taylor & Francis Ltd and its content may not be copied or emailed to multiple sites or posted to a listserv without the copyright holder's express written permission. However, users may print, download, or email articles for individual use. This abstract may be abridged. No warranty is given about the accuracy of the copy. Users should refer to the original published version of the material for the full abstract. (Copyright applies to all Abstracts.) Behavioral family intervention is an effective form of intervention for the prevention and treatment of a wide range of emotional and behavioral problems in children. There is a growing need to address the accessibility of these services. This paper reviews the literature on self-directed interventions designed to help parents manage difficult child behaviors. Evidence regarding the efficacy of interventions is reviewed, and some of the difficulties associated with self-directed programs are discussed. The Self-directed Triple P and Teen Triple P-Positive Parenting Programs are highlighted as examples of efficacious and effective behavioral family interventions fitting into a larger multilevel model of family intervention. The discussion of the efficacy and effectiveness of self-directed Triple P has implications for service delivery of parenting programs. [ABSTRACT FROM AUTHOR] Copyright of Child & Family Behavior Therapy is the property of Taylor & Francis Ltd and its content may not be copied or emailed to multiple sites or posted to a listserv without the copyright holder's express written permission. However, users may print, download, or email articles for individual use. This abstract may be abridged. No warranty is given about the accuracy of the copy. Users should refer to the original published version of the material for the full abstract. (Copyright applies to all Abstracts.)
    3                                                                                                                                                                                                                                                                                                                                         The topic of this study was to evaluate state-dependent effects of diazepam on the frequency characteristics of 47-channel spontaneous EEG maps. A novel method, the FFT-Dipole-Approximation was used to study effects on the strength and the topography of the maps in the different frequency bands. Map topography was characterized by the 3-dimensional location of the equivalent dipole source and map strength was defined as the spatial standard deviation (the Global Field Power) of the maps of each frequency point. The Global Field Power can be considered as a measure of the amount of energy produced by the system, while the source location gives an estimate of the center of gravity of all sources in the brain that were active at a certain frequency. State-dependency was studied by evaluating the drug effects before and after a continuous performance task of 25 min duration. Clear interactions between drug (diazepam vs. placebo) and time after drug intake (before and after the task) were found, especially in the inferior-superior location of the dipole sources. It supports the hypothesis that diazepam, like other drugs, has different effects on brain functions depending on the momentary functional state of the brain. In addition to the drug effects, clearly different source locations and Global Field Power were found for the different frequency bands, replicating earlier reports. Copyright <ef><bf><bd> 2011 Elsevier B. V., Amsterdam. All Rights Reserved The topic of this study was to evaluate state-dependent effects of diazepam on the frequency characteristics of 47-channel spontaneous EEG maps. A novel method, the FFT-Dipole-Approximation was used to study effects on the strength and the topography of the maps in the different frequency bands. Map topography was characterized by the 3-dimensional location of the equivalent dipole source and map strength was defined as the spatial standard deviation (the Global Field Power) of the maps of each frequency point. The Global Field Power can be considered as a measure of the amount of energy produced by the system, while the source location gives an estimate of the center of gravity of all sources in the brain that were active at a certain frequency. State-dependency was studied by evaluating the drug effects before and after a continuous performance task of 25 min duration. Clear interactions between drug (diazepam vs. placebo) and time after drug intake (before and after the task) were found, especially in the inferior-superior location of the dipole sources. It supports the hypothesis that diazepam, like other drugs, has different effects on brain functions depending on the momentary functional state of the brain. In addition to the drug effects, clearly different source locations and Global Field Power were found for the different frequency bands, replicating earlier reports. Copyright <ef><bf><bd> 2011 Elsevier B. V., Amsterdam. All Rights Reserved
    4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           The article reviews the book "Helping Children with Selective Mutism: A Guide for School-Based Professionals," by Christopher A. Kearney The article reviews the book "Helping Children with Selective Mutism: A Guide for School-Based Professionals," by Christopher A. Kearney
    5 STUDY OBJECTIVES: Our ongoing longitudinal study has shown that NREM delta (1-4 Hz) and theta (4-8 Hz) power measured at C3 and C4 decrease by more than 60% between ages 11 and 17 years. Here, we investigate the age trajectories of delta and theta power at frontal, central, and occipital electrodes DESIGN: Baseline sleep EEG was recorded twice yearly for 6 years in 2 cohorts, spanning ages 9-18 years, with overlap at 12-15 years SETTING: Sleep EEG was recorded in the subjects' homes with ambulatory recorders PARTICIPANTS: Sixty-seven subjects in 2 cohorts, one starting at age 9 (n = 30) and one at age 12 years (n = 37) MEASUREMENTS AND RESULTS: Sleep EEG recorded from Fz, Cz, C3, C4, and O1 was referred to mastoids. Visual scoring and artifact elimination was followed by FFT power analysis. Delta and theta EEG power declined steeply across this age range. The maturational trajectories of delta power showed a "back to front" pattern, with O1 delta power declining earliest and Fz delta power declining latest. Theta EEG power did not show this topographic difference in the timing of its decline. Delta, and to a lesser extent, theta power became frontally dominant in early adolescence CONCLUSIONS: We maintain our interpretation that the adolescent decline in EEG power reflects a widespread brain reorganization driven by synaptic pruning. The late decline in frontally recorded delta power indicates that plasticity is maintained in these circuits until a later age. Although delta and theta have similar homeostatic properties, they have different age and topographic patterns that imply different functional correlates STUDY OBJECTIVES: Our ongoing longitudinal study has shown that NREM delta (1-4 Hz) and theta (4-8 Hz) power measured at C3 and C4 decrease by more than 60% between ages 11 and 17 years. Here, we investigate the age trajectories of delta and theta power at frontal, central, and occipital electrodes DESIGN: Baseline sleep EEG was recorded twice yearly for 6 years in 2 cohorts, spanning ages 9-18 years, with overlap at 12-15 years SETTING: Sleep EEG was recorded in the subjects' homes with ambulatory recorders PARTICIPANTS: Sixty-seven subjects in 2 cohorts, one starting at age 9 (n = 30) and one at age 12 years (n = 37) MEASUREMENTS AND RESULTS: Sleep EEG recorded from Fz, Cz, C3, C4, and O1 was referred to mastoids. Visual scoring and artifact elimination was followed by FFT power analysis. Delta and theta EEG power declined steeply across this age range. The maturational trajectories of delta power showed a "back to front" pattern, with O1 delta power declining earliest and Fz delta power declining latest. Theta EEG power did not show this topographic difference in the timing of its decline. Delta, and to a lesser extent, theta power became frontally dominant in early adolescence CONCLUSIONS: We maintain our interpretation that the adolescent decline in EEG power reflects a widespread brain reorganization driven by synaptic pruning. The late decline in frontally recorded delta power indicates that plasticity is maintained in these circuits until a later age. Although delta and theta have similar homeostatic properties, they have different age and topographic patterns that imply different functional correlates
      human_code
    1          0
    2          0
    3          0
    4          0
    5          0

### Load relevant packages

We load the required packages.

``` r
# Loading packages
library(AIscreenR)  # Used to screen and calculate gpt vs. human performance
library(tibble)     # Used to work with tibbles
library(dplyr)      # Used to manipulate data
library(purrr)      # For loops 
library(usethis)    # Used to add the API key the R environment (only relevant the first time you screen)
library(future)     # Used to conduct screenings in parallel
```

### Prompting

The user should define a clear and specific prompt to guide the model’s
screening decisions. Below is an example prompt for identifying studies
related to Functional Family Therapy (FFT).

``` r
prompt <- "Evaluate the following study based on the selection criteria
 for a systematic review on the effects of family-based interventions on drug abuse
 reduction for young people in treatment for non-opioid drug use.
 A family-based intervention (FFT) is equivalent to a behavior focused
 family therapy, where young people’s drug use is understood in relation to family
 behavior problems. Family-based interventions also includes manual-based family therapies as
 it targets young people and their families as a system throughout treatment, and thereby recognizes
 the important role of the family system in the development and treatment of young people’s drug use
 problems. FFT was developed in the late 1980s on request from the US National Institute on Drug Abuse
 (NIDA). The development of FFT was initially heavily inspired by the alcohol abuse program
 Community Reinforcement Approach (CRA), which was aimed at restructuring the environment
 to reinforce non-alcohol associated activities. FFT developed to have more emphasis on
 contingency contracting, impulse control strategies specific to drug use,
 and increased emphasis on involvement of family members in treatment.
 FFT is designed to accommodate diverse populations of youths with a variety of behavioral,
 cultural and individual preferences. FFT has evolved for use in severe behavioral disturbances
 known to co-exist with substance use and dependence, and the core interventions
 have been enhanced to address several mental health related problems commonly occurring
 as comorbid conditions in drug use treatment participant.  For each study,
 I would like you to assess:  1) Is the study about a family-based intervention,
 such as Functional Family Therapy, Multidimensional Family Therapy, or
 Behavioral Family Therapy? (Outpatient manual-based interventions of any
 duration delivered to young people and their families). If not, exclude study.
 2) Are the participants in outpatient drug treatment primarily
 for non-opioid drug use? 3) Are the participants within age 11–21?"
```

### Running tabscreen_ollama()

We can now run the
[`tabscreen_ollama()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_ollama.md)
function with our data and prompt. For this example, we use the
`llama3.2:latest` model, which supports function-calling. We also choose
not to get explanations for the decisions (decision_description =
FALSE). A lot of other arguments are available, please see the function
documentation for details.

``` r
# Set parallel processing plan
future::plan(future::multisession)

# Running tabscreen_ollama
result_object <- tabscreen_ollama(
  data = filges2015_dat,      # data.frame / tibble with title & abstract
  prompt = prompt,            # screening prompt
  studyid = studyid,          # column with unique study IDs
  title = title,              # column with study titles
  abstract = abstract,        # column with study abstracts
  model = "llama3.2:latest",  # specify the local Ollama model to use
)

#> Progress: ───────────────────────────────────────────────────────────────── 100%

# Back to the sequential plan
plan(sequential)
```

## Get the results

The results are stored in a list with several components. The main
output from the GPT is in `results$answer_data`. Here we can select and
display relevant columns to compare human and model decisions. Other
components include `error_data` (failed requests) and, if reps \> 1,
`answer_data_aggregated` (summary per study across reps). From the
`results` object, we can also see metadata such as `run_date` and
`arg_list`.

``` r
# Select the relevant output columns

results$answer_data |> dplyr::select(author, human_code, decision_gpt, decision_binary)
#> A tibble: 5 × 4
#>  author                                 human_code decision_gpt decision_binary
#>  <chr>                                       <dbl> <chr>                  <dbl>
#> 1 Holloway R G and Gramling R and Kelly…          0 1                          1
#> 2 Morawska Alina and Stallman Helen M a…          0 0                          0
#> 3 Michel C M and Pascual-Marqui R D and…          0 0                          0
#> 4 Paul Howard A                                   0 0                          0
#> 5 Feinberg I and De Bie E and Davis N M…          0 1                          1
```

Here we compare the human screening decisions with the model’s
decisions. Use results\$answer_data to inspect individual responses. Key
columns are decision_gpt (0 = exclude, 1 = include, 1.1 = unsure) and
decision_binary (0 = exclude, 1 = include). If you set reps \> 1, the
results will include answer_data_aggregated and final_decision_gpt
(aggregated decision), which help assess stability across repeated
queries. Increasing reps improves reliability of incl_p but increases
runtime and local resource use. Model behaviour depends on the chosen
model, prompt wording, and other parameters. Experiment with prompt
wording, model choice, and the incl_cutoff threshold to improve
screening accuracy for your task. Consider setting decision_description
= TRUE so the model returns brief explanations (which can support
chain-of-thought reasoning). See [Chain of
thought](https://www.ibm.com/think/topics/chain-of-thoughts) for more
information.

## Conclusion

Some key advantages of using tabscreen_ollama() are that it keeps data
local for improved privacy, removes per-request billing (costs are
limited to hardware and maintenance), gives full control over model
choice (including custom or fine-tuned models), and improves
reproducibility by allowing model versions to be pinned and runs to be
performed offline. It is however important to note that local models may
require substantial compute resources, and may not be as up-to-date as
cloud-hosted models. Furthermore small models which are easier and more
convenient to run locally may not perform as well as larger models
available in the cloud.
