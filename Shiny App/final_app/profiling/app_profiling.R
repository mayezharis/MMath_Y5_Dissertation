library(profvis)

# profiling my own app
profvis({runApp('C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/final_app/md_plots_app.R')}, 
        prof_output = 'C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/final_app/profiling/md_app_prof')



# profiling Simon's ggplot app
profvis({runApp('C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/final_app/tests_and_others/simon_app_ggplot.R')}, 
        prof_output = 'C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/final_app/profiling/app_prof_test_ggplot')


# profiling Simon's base app
profvis({runApp('C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/final_app/tests_and_others/simon_example_app.R')}, 
        prof_output = 'C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/final_app/profiling/app_prof_test_base')
