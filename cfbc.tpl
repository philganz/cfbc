//Combining the Cohen-Fishman growth increment model 
//with a Box-Cox transformation: flexibility and uncertainty
//
//Terrance J. Quinn II and Richard B. Deriso
//
//Example with Pacific halibut mark-recapture data
//Implemented in ADMB by Phil Ganz

DATA_SECTION
  //Number of observations
  init_int    nobs;
  //Mark-recapture data 
  init_matrix data(1,nobs,1,3);
  
  //Create vectors for data columns
  vector Y1(1,nobs);
  vector Y2(1,nobs);
  vector Delt(1,nobs);
  //Assign values to vectors of interest using C++ code
  !! Y1 = column(data,1);
  !! Y2 = column(data,2);
  !! Delt = column(data,3);

  int i;

INITIALIZATION_SECTION
  gamma 0.1
  //alpha 0.95836
  rho   0.1

PARAMETER_SECTION
  init_number gamma;
  init_number alpha;
  init_number rho;
  
  number sigma_eps_sq;
  
  //Box-Cox transforms
  vector x1(1,nobs);
  vector x2(1,nobs);
  
  vector mu_x2(1,nobs);
  //vector sigma_x2_sq(1,nobs);

  vector tau_sq(1,nobs);

  //vector pred_Y2(1,nobs);
  number                   test_obj_fun;
  objective_function_value obj_fun;

PROCEDURE_SECTION
  //cout<<data<<endl;
  //cout<<Y1<<endl;
  //cout<<Y2<<endl;
  //cout<<Delt<<endl;
  //exit(42);

//Get Box-Cox transforms
  x1 = (pow(Y1,gamma)-1.)/gamma;
  x2 = (pow(Y2,gamma)-1.)/gamma;

//Expected value of x2
  for (i=1;i<=nobs;i++){
  mu_x2(i) = alpha * (1.-pow(rho,Delt(i))) / (1.-rho) + pow(rho,Delt(i)) * x1(i);}
 
  tau_sq = (1.-pow(rho,2*Delt)) / (1.-square(rho));

  sigma_eps_sq = 1./nobs * sum(elem_div(square(x2-mu_x2),tau_sq));

  //sigma_x2_sq = sigma_eps_sq * tau_sq;

  //pred_Y2 = pow(gamma*mean(x2)+1,1/gamma);

  //cout<<"rho"<<endl;
  //cout<<rho<<endl;
  //cout<<"pow(rho,Delt)"<<endl;
  //cout<<pow(rho,Delt)<<endl;
  //exit(42);

  //obj_fun = square(1-1);
  obj_fun = nobs * (log(2.*PI*sigma_eps_sq)+1.) + sum(log(tau_sq)) - 2.*(gamma-1.)*sum(log(Y2));
  //obj_fun = nobs * (log(2.*PI*sigma_eps_sq)+1.) + sum(log(tau_sq)) - 2.*(gamma-1.)*sum(log(Y2));

REPORT_SECTION
  report<<"gamma"<<endl;
  report<<gamma<<endl;
  report<<"alpha"<<endl;
  report<<alpha<<endl;
  report<<"rho"<<endl;
  report<<rho<<endl;
  report<<"sigma_eps_sq"<<endl;
  report<<sigma_eps_sq<<endl; 
  report<<"test_obj_fun"<<endl;
  report<<test_obj_fun<<endl;
  report<<"obj_fun"<<endl;
  report<<obj_fun<<endl;  
  report<<"x1"<<endl;
  report<<x1<<endl;
  report<<"x2"<<endl;
  report<<x2<<endl;  
  report<<"mu_x2"<<endl;
  report<<mu_x2<<endl;
  report<<"tau_sq"<<endl;
  report<<tau_sq<<endl;
  report<<"residuals"<<endl;
  report<<x2-mu_x2<<endl;
